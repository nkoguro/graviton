(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body (html:div :id "div-block")))
      ()
    ;; Sets "Hello, world" in the div element.
    (let1 div (document'get-element-by-id "div-block")
      (set! (~ div'inner-text) "Hello, world"))
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))

