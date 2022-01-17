(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body "Hello,world"))
      ()
    (jsevent-await window "keyup" ())
    (grv-exit 0)))
