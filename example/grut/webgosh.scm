(use gauche.logger)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use srfi-13)
(use text.html-lite)
(use util.match)

(define (read-multi-line text prompter validator)
  (let loop ((current-prompt (prompter 0))
             (line-alist '())
             (row 0)
             (pos 0))
    (receive (line key) (text'read-line :prompt (string-append "\x1b;[2K\x1b;[1G" current-prompt)
                                        :content (assv-ref line-alist row "")
                                        :position pos)
      (let1 line-alist (assv-set! line-alist row line)
        (match key
          ("ArrowUp"
           (let ((next-row (max 0 (- row 1)))
                 (next-pos (- (text'cursor-column) (string-length current-prompt))))
             (when (< next-row row)
               (text'print-text "\x1b;[F"))
             (loop (prompter next-row) line-alist next-row next-pos)))
          ("ArrowDown"
           (let ((next-row (min (+ row 1) (- (length line-alist) 1)))
                 (next-pos (- (text'cursor-column) (string-length current-prompt))))
             (when (< row next-row)
               (text'print-text "\x1b;[E"))
             (loop (prompter next-row) line-alist next-row next-pos)))
          ("Enter"
           (let1 str (string-join (map cdr (sort line-alist (^(p1 p2) (< (car p1) (car p2))))) "\n")
             (cond
               ((validator str)
                (dotimes ((- (length line-alist) row))
                  (text'print-text "\n"))
                str)
               (else
                (text'print-text "\n")
                (loop (prompter (+ row 1)) line-alist (+ row 1) 0)))))
          ("C-c"
           (text'print-text "\n")
           #f))))))

(define (read-sexpr text)
  (let1 str (read-multi-line text (lambda (row)
                                    (if (= row 0)
                                      "webgosh>"
                                      " (cont)>"))
                             (lambda (str)
                               (guard (e ((and (<read-error> e)
                                               (string-contains (condition-message e "") "EOF inside a list"))
                                          #f)
                                         (else
                                          #t))
                                 (read-from-string str)
                                 #t)))
    (if str
      (guard (e (else (values #f e)))
        (values (read-from-string str) #f))
      (values (eof-object) #f))))

;; read-eval-print-loop uses with-error-handler, but it can be incompatible with partial continuation.
(define (repl)
  (define (reader)
    (receive (expr err) (read-sexpr text)
      (cond
        (err
         (beep 1000 0.1)
         (report-error err)
         (reader))
        ((eof-object? expr)
         (reader))
        (else
         (evaluator expr)))))
  (define (evaluator expr)
    (receive (vals err) (guard (e (else (values #f e)))
                          (values (values->list (eval expr (with-module gauche.internal (vm-current-module)))) #f))
      (cond
        (err
         (beep 1000 0.1)
         (report-error err)
         (reader))
        (else
         (printer vals)))))
  (define (printer vals)
    (for-each (lambda (v)
                (format #t "~s~%" v))
              vals)
    (reader))
  (reader))

(define *text-width* #f)
(define *text-height* #f)
(define *font-size* #f)

(define-grut-window
  (text :id text :width *text-width* :height *text-height* :font-size *font-size* :overflow-y "scroll")
  (canvas :id canvas :context-2d ctx :width 1000 :height 1000)
  :theme 'dark :title "gosh on Web" :margin "0 0 0 5")

(define (main args)
  (let-args (cdr args) ((use-browser? "b|browser" #f)
                        (font-size "font-size=s" #f)
                        (width "w|width=i" #f)
                        (height "h|height=i" #f))
    (if use-browser?
      (grv-browser)
      (grv-player :resizable? #t))

    (set! *text-width* width)
    (set! *text-height* height)
    (set! *font-size* font-size))

  (grv-log-config :log-level 1)
  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (log-format "Exit by Escape key")
        (grv-exit)))

    (grv-text-key-map'bind "ArrowUp" "complete-line-edit")
    (grv-text-key-map'bind "ArrowDown" "complete-line-edit")
    (grv-text-key-map'bind "C-c" "complete-line-edit")

    (call-with-output-grv-text text
      (lambda (out)
        (parameterize ((current-output-port out)
                       (current-error-port out))
          (print "Welcome to gosh on Web!\n"
                 "Press Escape to exit.\n")
          (repl))))))
