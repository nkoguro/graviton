(use file.util)
(use gauche.logger)
(use gauche.parameter)
(use gauche.parseopt)
(use gauche.vport)
(use graviton)
(use graviton.interactive)
(use graviton.grut)
(use srfi-1)
(use srfi-13)
(use text.html-lite)
(use util.match)

(bind-url-path "/webgosh.css" (build-path (sys-dirname (current-load-path)) "webgosh.css"))

(define (input-continues? str)
  (cond
    ((#/^\s*,\S+/ str)
     #f)
    (else
     (guard (e ((and (<read-error> e)
                     (string-contains (condition-message e "") "EOF inside a list"))
                #t)
               (else
                #f))
       (read-from-string str)
       #f))))

(define (get-text-output-port ui-worker)
  (make <virtual-output-port>
    :putb (lambda (b)
            (worker-fire-event ui-worker 'putb b))
    :puts (lambda (s)
            (worker-fire-event ui-worker 'puts s))))

(define (get-text-input-port grv-text out)
  (let1 in #f
    (make <virtual-input-port>
      :getb (lambda ()
              (let loop ((b (if in
                              (read-byte in)
                              (eof-object))))
                (cond
                  ((eof-object? b)
                   (set! in (open-input-string (rlet1 str (get-input-text grv-text #t)
                                                 (display str out)
                                                 (flush out))))
                   (loop (read-byte in)))
                  (else
                   b)))))))

(define *history-size* 3)

(dotimes (i *history-size*)
  (eval `(define-in-module gauche ,(string->symbol (format "*~d" (+ i 1))) #f) (find-module 'gauche)))

(define (record-history v)
  (let loop ((i 1)
             (sexprs '()))
    (cond
      ((< *history-size* i)
       (eval `(begin ,@sexprs) (find-module 'gauche)))
      ((= i 1)
       (loop (+ i 1) (cons `(set! *1 ,(cond
                                        ((or (symbol? v) (pair? v))
                                         (list 'quote v))
                                        (else
                                         v)))
                           sexprs)))
      (else
       (loop (+ i 1) (cons `(set! ,(string->symbol (format "*~d" i)) ,(string->symbol (format "*~d" (- i 1)))) sexprs))))))

(define (print-history)
  (dotimes (i *history-size*)
    (format #t "*~d: ~s~%" (+ i 1) (global-variable-ref (find-module 'gauche) (string->symbol (format "*~d" (+ i 1))) #f))))

(define (make-prompt env)
  (let* ((worker (env-target-worker env))
         (module (worker-current-module worker))
         (sandbox (worker-sandbox-module worker))
         (prompt (cond
                   ((equal? module sandbox)
                    "gosh$ ")
                   (else
                    (format "gosh[~a]$ " (module-name module))))))
    (list prompt (string-append (make-string (- (string-length prompt) 1) #\.) " "))))

(define (update-status! status env)
  (let1 worker (env-target-worker env)
    (clear-screen status)
    (format status "~a: ~s" (worker-eval '(~ document'title) worker) worker)))

(define-class <environ> ()
  ((target-worker-stack :init-form (list (current-worker)))))

(define (env-target-worker env)
  (let1 stack (~ env'target-worker-stack)
    (cond
      ((null? stack)
       (format (current-error-port) "No workers found. Aborted.")
       (close-window))
      ((worker-active? (car stack))
       (car stack))
      (else
       (set! (~ env'target-worker-stack) (cdr stack))
       (errorf "~s is inactive, detatched." (car stack))))))

(define (env-attach! env worker)
  (push! (~ env'target-worker-stack) worker))

(define (env-detatch! env)
  (when (< 1 (length (~ env'target-worker-stack)))
    (pop! (~ env'target-worker-stack))))

(define (main args)
  (let-args (cdr args) ((use-browser? "b|browser" #f)
                        (font-size "font-size=s" #f))
    (grv-log-config :log-level 1)

    (if use-browser?
      (grv-config :client 'browser)
      (grv-config :client 'player))

    (with-window
        (grv-window
          :css "/webgosh.css"
          :title "gosh on Web"
          :body
          (html:body
           :class "grut-monospace-font"
           (html:div
            :id "container"
            :style (alist->style `(("font-size" . ,font-size)))
            (html:grv-text :id "console")
            (html:grv-text :id "status"))))
        (console status)
      (show-cursor console)

      (on-event 'putb (b)
        (write-byte b console))

      (on-event 'puts (s)
        (display s console))

      (let* ((out (get-text-output-port (current-worker)))
             (in (get-text-input-port console out))
             (env (make <environ>)))
        (define (%eval* sexpr success fail record?)
          (worker-eval* sexpr
                        (env-target-worker env)
                        (lambda vals
                          (when (and record? (not (null? vals)))
                            (record-history (first vals)))
                          (apply success vals))
                        fail
                        :input-port in
                        :output-port out
                        :error-port out
                        :trace-port out))

        (parameterize ((current-output-port console)
                       (current-error-port console)
                       (current-trace-port console))
          (while #t
            (guard (e (else (format #t "*** ~a~%" (condition-message e))))
              (update-status! status env)

              (let1 str (read-text/edit console
                                        :prompt (make-prompt env)
                                        :input-continues input-continues?)
                (cond
                  ((#/^\s*,((\S+)(\s+(.*))?)/ str)
                   => (lambda (m)
                        (let ((toplevel-command (m 2))
                              (arg-str (or (m 4) "")))
                          (cond
                            ((equal? toplevel-command "attach")
                             (%eval* (read-from-string arg-str)
                                     (match-lambda*
                                       (((? (cut is-a? <> <worker>) worker) rest ...)
                                        (write worker)
                                        (newline)
                                        (env-attach! env worker))
                                       (vals
                                        (errorf "<worker> required, but got ~s" vals)))
                                     (lambda (e)
                                       (report-error e))
                                     #t))
                            ((equal? toplevel-command "detach")
                             (env-detatch! env))
                            ((equal? toplevel-command "lsw")
                             (print-main-worker-list))
                            ((equal? toplevel-command "sm")
                             (%eval* `(select-module ,(cond
                                                        ((and arg-str (< 0 (string-length arg-str)))
                                                         (string->symbol arg-str))
                                                        (else
                                                         (module-name (worker-sandbox-module (env-target-worker env))))))
                                     (lambda _ #t)
                                     (^e (report-error e))
                                     #f))
                            ((equal? toplevel-command "cm")
                             (write (worker-current-module (env-target-worker env)))
                             (newline))
                            ((equal? toplevel-command "history")
                             (print-history))
                            (else
                             (errorf "syntax error: ~a" (m 0)))))))
                  (else
                   (port-for-each (lambda (sexpr)
                                    (%eval* sexpr
                                            (lambda vals
                                              (for-each (lambda (v)
                                                          (write v)
                                                          (newline))
                                                        vals))
                                            (lambda (e)
                                              (report-error e))
                                            #t))
                                  (let1 in (open-input-string str)
                                    (cut read in)))))))))))))
