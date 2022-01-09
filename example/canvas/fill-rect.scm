(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (grut-canvas-window 400 200)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (set! (~ ctx'fill-style) "green")
      (ctx'fill-rect 10 10 100 100))))
