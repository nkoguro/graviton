(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body (html:div :id "div-block")))
      ()
    ;; Sets "Hello, world" in the div element and prints the body in HTML.
    (print (jslet/await ((text "Hello, world"))
             (let1 div (document.getElementById "div-block")
               (set! div.innerText text)
               (respond document.body.innerHTML))))
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))
