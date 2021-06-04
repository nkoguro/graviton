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

(define (eval-worker in out)
  (let ((module (make-module #f)))
    (eval '(import gauche) module)

    (on-event 'eval (str)
      (parameterize ((current-input-port in)
                     (current-output-port out)
                     (current-error-port out))
        (begin0
            (guard (e (else (list 'error e)))
              (list 'success (values->list (eval (read-from-string str) module))))
          (set! module (with-module gauche.internal (vm-current-module))))))))

(define (main args)
  (let-args (cdr args) ((use-browser? "b|browser" #f)
                        (font-size "font-size=s" #f)
                        (width "w|width=i" #f)
                        (height "h|height=i" #f))
    (grv-window
      :path "/"
      :head (html:title "gosh on Web")
      :body
      (html:body
       :style "color:white; background-color:black; margin: 0 0 0 5"
       (html:grv-text :id "text"
                      :column width
                      :row height
                      :class "grut-monospace-font"
                      :style (string-join (append '("overflow-y: scroll"
                                                    "height: 100vh")
                                                  (if font-size `(,#"font-size:~|font-size|") '()))
                                          ";")))

      (let-elements (text)
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
               (report-error e text)))))))

    (grv-log-config :log-level 1)

    (if use-browser?
      (grv-start-server)
      (grv-start-player :resizable? #t))))
