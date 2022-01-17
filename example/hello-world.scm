(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body "Hello,world"))
      ()
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (grv-exit 0)))
