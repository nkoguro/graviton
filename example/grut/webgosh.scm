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

(define (make-prompt worker)
  (let* ((module (worker-current-module worker))
         (sandbox (worker-sandbox-module worker))
         (prompt (cond
                   ((equal? module sandbox)
                    "gosh$ ")
                   (else
                    (format "gosh[~a]$ " (module-name module))))))
    (list prompt (string-append (make-string (- (string-length prompt) 1) #\.) " "))))

(define (main args)
  (let-args (cdr args) ((use-browser? "b|browser" #f)
                        (font-size "font-size=s" #f))
    (grv-log-config :log-level 1)

    (if use-browser?
      (grv-config :client 'browser)
      (grv-config :client 'player))

    (with-window
        (grv-window
          :title "gosh on Web"
          :body
          (html:body
           :style "color:white; background-color:black; margin: 0 0 0 5"
           (html:grv-text :id "text"
                          :class "grut-monospace-font"
                          :style (string-join (append '("overflow-y: scroll"
                                                        "height: 100vh")
                                                      (if font-size `(,#"font-size:~|font-size|") '()))
                                              ";"))))
        (text)
      (show-cursor text)

      (on-event 'putb (b)
        (write-byte b text))

      (on-event 'puts (s)
        (display s text))

      (let* ((out (get-text-output-port (current-worker)))
             (in (get-text-input-port text out))
             (target-worker-stack (list (current-worker))))
        (define (%eval* sexpr success fail record?)
          (worker-eval* sexpr
                        (car target-worker-stack)
                        (lambda vals
                          (when (and record? (not (null? vals)))
                            (record-history (first vals)))
                          (apply success vals))
                        fail
                        :input-port in
                        :output-port out
                        :error-port out
                        :trace-port out))

        (parameterize ((current-output-port text)
                       (current-error-port text)
                       (current-trace-port text))
          (while #t
            (guard (e (else (format text "*** ~a~%" (condition-message e))))
              (let1 str (read-text/edit text
                                        :prompt (make-prompt (car target-worker-stack))
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
                                        (write worker text)
                                        (newline text)
                                        (push! target-worker-stack worker))
                                       (vals
                                        (errorf "<worker> required, but got ~s" vals)))
                                     (lambda (e)
                                       (report-error e text))
                                     #t))
                            ((equal? toplevel-command "detach")
                             (unless (= (length target-worker-stack) 1)
                               (pop! target-worker-stack)))
                            ((equal? toplevel-command "lsw")
                             (print-main-worker-list))
                            ((equal? toplevel-command "sm")
                             (%eval* `(select-module ,(cond
                                                        ((and arg-str (< 0 (string-length arg-str)))
                                                         (string->symbol arg-str))
                                                        (else
                                                         (module-name (worker-sandbox-module
                                                                        (car target-worker-stack))))))
                                     (lambda _ #t)
                                     (^e (report-error e text))
                                     #f))
                            ((equal? toplevel-command "cm")
                             (write (worker-current-module (car target-worker-stack)) text)
                             (newline text))
                            ((equal? toplevel-command "history")
                             (print-history))
                            (else
                             (errorf "syntax error: ~a" (m 0)))))))
                  (else
                   (port-for-each (lambda (sexpr)
                                    (let1 target-worker (car target-worker-stack)
                                      (unless (worker-active? target-worker)
                                        (cond
                                          ((= (length target-worker-stack) 1)
                                           (error "No workers in the stack"))
                                          (else
                                           (pop! target-worker-stack)
                                           (errorf "~s is inactive, will switch to ~s" target-worker (car target-worker-stack)))))
                                      (%eval* sexpr
                                              (lambda vals
                                                (for-each (lambda (v)
                                                            (write v text)
                                                            (newline text))
                                                          vals))
                                              (lambda (e)
                                                (report-error e text))
                                              #t)))
                                  (let1 in (open-input-string str)
                                    (cut read in)))))))))))))
