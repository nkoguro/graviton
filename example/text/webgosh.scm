(use gauche.logger)
(use gauche.parseopt)
(use graviton)
(use srfi-13)
(use text.html-lite)
(use util.match)

(define (beep freq len)
  (let1 oscillator (audio-context'create-oscillator)
    (set! (~ oscillator'type) "square")
    (set! (~ oscillator'frequency'value) freq)
    (oscillator'connect (~ audio-context'destination))
    (let1 now (~ audio-context'current-time)
      (oscillator'start now)
      (oscillator'stop (+ now len)))))

(define (read-sexpr text)
  (let ((prompt      "webgosh>")
        (cont-prompt " (cont)>")
        (str ""))
    (let loop ((prompt prompt))
      (receive (sexpr state) (guard (e ((and (<read-error> e)
                                             (string-contains (condition-message e "") "EOF inside a list"))
                                        (set! str (string-append str "\n"))
                                        (values #f 'continue))
                                       (else (values #f e)))
                               (set! str (string-append str (text'read-line :prompt prompt)))
                               (values (read-from-string str) 'done))
        (match state
          ('continue
           (loop cont-prompt))
          ('done
           sexpr)
          (err
           (beep 1000 0.1)
           (raise err)))))))

(define (main args)
  (let-args (cdr args) ((use-browser? "b|browser" #f)
                        (font-size "font-size=s" "16px"))
    (if use-browser?
      (grv-browser)
      (grv-player :resizable? #t))

    (define-document-content
      (html:body :style "background-color: black"
                 (html:grv-text :style #"font-size: ~font-size; width: auto; height: auto"))))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (log-format "Exit by Escape key")
        (grv-exit)))

    (let ((text (document'query-selector "grv-text")))
      (call-with-output-grv-text text
        (lambda (out)
          (parameterize ((current-output-port out)
                         (current-error-port out))
            (print "Welcome to gosh on Web!\n"
                   "Press Escape to exit.\n")
            (read-eval-print-loop
              (lambda ()
                (read-sexpr text))
              (lambda (expr env)
                (let1 vals (guard (e (else e))
                             (values->list (eval expr env)))
                  (if (list? vals)
                    (apply values vals)
                    (begin
                      (beep 1000 0.1)
                      (raise vals)))))
              (lambda vals
                (for-each (lambda (v)
                            (format out "~s~%" v))
                          vals))
              (lambda () #f))))))))
