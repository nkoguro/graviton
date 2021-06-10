(use gauche.logger)
(use gauche.parameter)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use srfi-13)
(use text.html-lite)
(use util.match)

(define (input-continues? str)
  (guard (e ((and (<read-error> e)
                  (string-contains (condition-message e "") "EOF inside a list"))
             #t)
            (else
             #t))
    (read-from-string str)
    #f))

(define make-user-module
  (let1 id 0
    (lambda ()
      (let* ((name (begin0
                       (string->symbol (format "user~a" id))
                     (inc! id)))
             (module (make-module name)))
        (eval '(import gauche) module)
        (eval '(begin (use graviton) (use graviton.grut)) module)
        module))))

(define (eval-worker in out)
  (let* ((sandbox-module (make-module #f))
         (curmod sandbox-module))
    (eval '(import gauche) sandbox-module)
    (eval '(begin (use graviton) (use graviton.grut)) sandbox-module)

    (current-input-port in)
    (current-output-port out)
    (current-error-port out)

    (on-event 'eval (str)
      (guard (e (else (list 'error e)))
        (match (read-from-string str)
          (('select-module name)
           (set! curmod (find-module name))
           (list 'success '()))
          (sexpr
           (list 'success (values->list (eval sexpr curmod)))))))))

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
      (let1 evaluator (run-worker-thread (cut eval-worker (get-text-input-port text) text))
        (while #t
          (match (worker-call-event evaluator 'eval (begin0
                                                        (read-text/edit text
                                                                        :prompt '("webgosh>" "........")
                                                                        :input-continues input-continues?)
                                                      (newline text)))
            (('success vals)
             (for-each (lambda (v)
                         (write v text)
                         (newline text))
                       vals))
            (('error e)
             (report-error e text))))))))
