(use file.util)
(use gauche.hook)
(use gauche.logger)
(use gauche.parameter)
(use gauche.parseopt)
(use gauche.sequence)
(use gauche.vport)
(use graviton)
(use graviton.interactive)
(use graviton.grut)
(use srfi-1)
(use srfi-13)
(use text.html-lite)
(use util.match)

(define css-path (build-path (sys-dirname (current-load-path)) "webgosh.css"))

(define (input-continues? input-context)
  (let1 str (input-context-text-content input-context)
    (rlet1 continue? (cond
                       ((#/^\s*,\S+/ str)
                        #f)
                       (else
                        (guard (e ((and (<read-error> e)
                                        (string-contains (condition-message e "") "EOF inside a list"))
                                   #t)
                                  (else
                                   #f))
                          (read-from-string str)
                          #f)))
      (unless continue?
        (end-completion input-context)))))

(define (get-text-output-port ui-worker)
  (make <virtual-output-port>
    :putb (lambda (b)
            (ui-worker'putb b))
    :puts (lambda (s)
            (ui-worker'puts s))))

(define (get-text-input-port grut-text out)
  (let1 in #f
    (make <virtual-input-port>
      :getb (lambda ()
              (let loop ((b (if in
                              (read-byte in)
                              (eof-object))))
                (cond
                  ((eof-object? b)
                   (set! in (open-input-string (rlet1 str (get-input-text grut-text #t)
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
  ((target-worker-stack :init-form (list (current-worker)))
   (input-port :init-keyword :input-port)
   (output-port :init-keyword :output-port)
   (console :init-keyword :console)))

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

(define (env-eval* env sexpr success fail record?)
  (worker-eval* sexpr
                (env-target-worker env)
                (lambda vals
                  (when (and record? (not (null? vals)))
                    (record-history (first vals)))
                  (apply success vals))
                fail
                :input-port (~ env'input-port)
                :output-port (~ env'output-port)
                :error-port (~ env'output-port)
                :trace-port (~ env'output-port)
                :console (~ env'console)))

;;;

(define (process-toplevel-command env command arg-str)
  (cond
    ((equal? command "attach")
     (env-eval* env
                (read-from-string arg-str)
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
    ((equal? command "detach")
     (env-detatch! env))
    ((equal? command "lsw")
     (print-main-worker-list))
    ((equal? command "sm")
     (env-eval* env `(select-module ,(cond
                                       ((and arg-str (< 0 (string-length arg-str)))
                                        (string->symbol arg-str))
                                       (else
                                        (module-name (worker-sandbox-module (env-target-worker env))))))
                (lambda _ #t)
                (^e (report-error e))
                #f))
    ((equal? command "cm")
     (write (worker-current-module (env-target-worker env)))
     (newline))
    ((equal? command "history")
     (print-history))
    ((or (equal? command "h") (equal? command "help"))
     (print "You're in REPL of Gauche Web shell (demo).")
     (print "Type a Scheme expression to evaluate.")
     (print "Evaluate (exit) to exit REPL.")
     (print "A word preceded with comma has special meaning.")
     (print "")
     (print " ,attach  Attach to the specified worker.")
     (print " ,detach  Detach from the current worker.")
     (print " ,lsw     Print the list of workers.")
     (print " ,sm      Switch to the module.")
     (print " ,cm      Print the current module.")
     (print " ,history Show REPL history."))
    (else
     (errorf "Invalid toplevel command: ~a" command))))

;;;

(define (extract-common-prefix strs)
  (let loop ((prefix (car strs))
             (rest (cdr strs)))
    (cond
      ((null? rest)
       prefix)
      (else
       (let* ((str (car rest))
              (i (string-prefix-length prefix str)))
         (loop (substring prefix 0 i) (cdr rest)))))))

(define-class <completion-state> ()
  ((row :init-keyword :row)
   (start-index :init-keyword :start-index)
   (all-words :init-value '())
   (candidates :init-value '())
   (input-word :init-value "")
   (select-row :init-value 0)))

(define (show-completion-window input-context column)
  (let ((console (~ input-context'data'console-element))
        (completion (~ input-context'data'completion-element)))
    (receive (x y w h) (compute-character-position&size console column (~ input-context'cursor-row))
      (let-jsproperties (completion'get-bounding-client-rect)
          ((completion-w width) (completion-h height))
        (let-jsproperties (console'get-bounding-client-rect)
            ((console-x x) (console-y y) (console-w width) (console-h height))
          (set! (~ completion'style'left) (min x (- (+ console-x console-w) completion-w)))
          (set! (~ completion'style'top) (let1 top (+ y h)
                                             (if (< (+ top h)  (+ console-y console-h))
                                               top
                                               (- y completion-h))))
          (set! (~ completion'style'visibility) "visible"))))))

(define (hide-completion-window input-context)
  (let ((completion (~ input-context'data'completion-element)))
    (set! (~ completion'style'visibility) "hidden")))

(define *word-char-set* #[!$%&*+\-./:<=>?@\^~0-9A-Za-z])

(define (start-completion input-context)
  (let* ((cursor-column (~ input-context'cursor-column))
         (cursor-row (~ input-context'cursor-row))
         (offset (~ input-context'offset))
         (line (input-context-text-line input-context))
         (start-index (+ (or (string-skip-right line *word-char-set* 0 (- cursor-column offset)) -1) 1))
         (start-column (+ start-index offset))
         (env (~ input-context'data'environ))
         (state (make <completion-state>
                  :row cursor-row
                  :start-index start-index)))
    (set! (~ input-context'data'completion-state) state)
    (env-eval* env
               '(let1 tbl (make-hash-table)
                  (for-each (lambda (sym)
                              (hash-table-put! tbl sym #t))
                            (hash-table-keys (module-table (current-module))))
                  (for-each (lambda (m)
                              (for-each (lambda (sym)
                                          (hash-table-put! tbl sym #t))
                                        (hash-table-keys (module-table m))))
                            (module-precedence-list (current-module)))
                  (for-each (lambda (m)
                              (for-each (lambda (sym)
                                          (hash-table-put! tbl sym #t))
                                        (module-exports m)))
                            (module-imports (current-module)))
                  (hash-table-keys tbl))
               (lambda (lst)
                 (set! (~ state'all-words) (sort (remove (^s (string-contains s " ")) (map symbol->string lst))))
                 (add-hook! (~ input-context'data'hook) update-completion-by-input)
                 (switch-keymap input-context (~ input-context'data'completion-keymap))
                 (update-completion input-context #t))
               (lambda (e)
                 #f)
               #f)))

(define *right-padding* 5)
(define *max-win-rows* 10)

(define (render-completion input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((completion (~ input-context'data'completion-element))
           (candidates (~ state'candidates))
           (win-cols (+ (apply max (map string-length candidates)) *right-padding*))
           (win-rows (min *max-win-rows* (length candidates))))
      (clear-screen completion)
      (set! (~ completion'column) win-cols)
      (set! (~ completion'row) win-rows)
      (for-each-with-index (lambda (i str)
                             (set-line-style! completion
                                              i
                                              "background-color"
                                              (if (= i (~ state'select-row))
                                                "blue"
                                                #f))
                             (display str completion)
                             (newline completion)
                             (reset-character-attribute completion))
                           candidates)
      (scroll-to completion (~ state'select-row) #f)
      (show-completion-window input-context (+ (~ state'start-index) (~ input-context'offset))))))

(define (insert-common-prefix input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((line (input-context-text-line input-context))
           (start (~ state'start-index))
           (end (- (~ input-context'cursor-column) (~ input-context'offset)))
           (str (substring line start end))
           (candidates (~ state'candidates))
           (prefix (extract-common-prefix (~ state'candidates)))
           (tail (if (< (string-length str) (string-length prefix))
                   (string-copy prefix (string-length str))
                   "")))
      (set! (~ state'input-word) prefix)
      (edit:insert-string input-context tail)
      (when (= (length candidates) 1)
        (end-completion input-context)))))

(define (update-completion input-context auto-insert?)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((row (~ state'row))
           (start (~ state'start-index))
           (end (- (~ input-context'cursor-column) (~ input-context'offset)))
           (line (input-context-text-line input-context row))
           (str (if (< start end)
                  (substring line start end)
                  ""))
           (completion (~ input-context'data'completion-element))
           (all-words (~ state'all-words))
           (candidates (filter (cut string-prefix? str <>) all-words)))
      (set! (~ state'candidates) candidates)
      (set! (~ state'input-word) str)
      (cond
        ((or (null? candidates) (= (string-length str) 0))
         (end-completion input-context))
        (auto-insert?
         (insert-common-prefix input-context)
         (if (= (length candidates) 1)
           (end-completion input-context)
           (render-completion input-context)))
        (else
         (render-completion input-context))))))

(define (update-completion-by-input input-context type str)
  (or (and-let1 state (~ input-context'data'completion-state)
        (let* ((row (~ state'row))
               (start-column (+ (~ state'start-index) (~ input-context'offset)))
               (cursor-column (~ input-context'cursor-column))
               (cursor-row (~ input-context'cursor-row)))
          (cond
            ((and (= row cursor-row)
                  (< start-column cursor-column))
             (update-completion input-context #f))
            (else
             (end-completion input-context))))
        #t)
      (end-completion input-context)))

(define (end-completion input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (hide-completion-window input-context)
    (switch-keymap input-context (global-keymap))
    (set! (~ input-context'data'completion-state) #f)
    (remove-hook! (~ input-context'data'hook) update-completion-by-input)))

(define (indent-or-completion input-context)
  (let ((state (~ input-context'data'completion-state))
        (str (string-copy (input-context-text-line input-context) 0 (- (~ input-context'cursor-column)
                                                                       (~ input-context'offset)))))
    (cond
      ((and state (null? (~ state'candidates)))
       (end-completion input-context))
      (state
       (insert-common-prefix input-context))
      ((= (string-length (string-trim-both str)) 0)
       ;; TODO: Add indent
       (edit:insert-string input-context "  "))
      (else
       (start-completion input-context)))))

(define (next-candidate input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (inc! (~ state'select-row))
    (when (<= (length (~ state'candidates)) (~ state'select-row))
      (set! (~ state'select-row) 0))))

(define (prev-candidate input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (dec! (~ state'select-row))
    (when (< (~ state'select-row) 0)
      (set! (~ state'select-row) (- (length (~ state'candidates)) 1)))))

(define (commit-completion input-context)
  (let* ((state (~ input-context'data'completion-state))
         (input-word (~ state'input-word))
         (select-row (~ state'select-row))
         (selected-word (list-ref (~ state'candidates) select-row))
         (tail (string-copy selected-word (string-length input-word))))
    (edit:insert-string input-context tail)
    (end-completion input-context)))

;;;

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (font-size "font-size=s" #f))
    (grv-log-config :log-level 1)

    (grv-config :client (cond
                          (force-player? 'player)
                          (force-browser? 'browser)
                          (else #f)))

    (with-window
        (grv-window
          :css (file->url css-path)
          :title "gosh on Web"
          :body
          (html:body
           :class "grut-monospace-font"
           (html:div
            :id "container"
            :style (alist->style `(("font-size" . ,font-size)))
            (html:grut-text :id "console")
            (html:grut-text :id "status"))
           (html:grut-text :id "completion" :style "visibility: hidden")))
        (console status completion)
      (show-cursor console)

      (define-message putb (b)
        (write-byte b console))

      (define-message puts (s)
        (display s console))

      (let* ((out (get-text-output-port (current-worker)))
             (in (get-text-input-port console out))
             (env (make <environ> :input-port in :output-port out :console console))
             (hook (make-hook 3))
             (completion-keymap (make-keymap (global-keymap))))
        (parameterize ((current-output-port console)
                       (current-error-port console)
                       (current-trace-port console))
          (bind-key (global-keymap) "Tab" indent-or-completion)

          (bind-key completion-keymap "ArrowUp" prev-candidate)
          (bind-key completion-keymap "ArrowDown" next-candidate)
          (bind-key completion-keymap "Escape" end-completion)
          (bind-key completion-keymap "Enter" commit-completion)

          (format #t "Gauche version ~a~%" (gauche-version))
          (format #t "You can use Tab Completion.~%~%")
          (while #t
            (guard (e (else (format #t "*** ~a~%" (condition-message e))))
              (update-status! status env)

              (let1 str (read-text/edit console
                                        :prompt (make-prompt env)
                                        :input-continues input-continues?
                                        :data-alist `((environ . ,env)
                                                      (console-element . ,console)
                                                      (completion-element . ,completion)
                                                      (completion-state . #f)
                                                      (completion-keymap . ,completion-keymap)
                                                      (hook . ,hook))
                                        :on-input (lambda (ctx type str)
                                                    (run-hook hook ctx type str)))
                (cond
                  ((#/^\s*,((\S+)(\s+(.*))?)/ str)
                   => (lambda (m)
                        (let ((command (m 2))
                              (arg-str (or (m 4) "")))
                          (process-toplevel-command env command arg-str))))
                  (else
                   (port-for-each (lambda (sexpr)
                                    (env-eval* env
                                               sexpr
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
