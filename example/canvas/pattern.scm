(use file.util)
(use graviton)
(use graviton.grut)

(bind-url-path "/" (sys-dirname (current-load-path)))

(define (main args)
  (with-window (grut-canvas-window 300 300)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (let1 pat (ctx'create-pattern (load-image "/Canvas_createpattern.png") "repeat")
        (set! (~ ctx'fill-style) pat)
        (ctx'fill-rect 0 0 300 300)))))
