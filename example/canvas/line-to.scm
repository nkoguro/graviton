(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (grut-canvas-window 300 150)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (ctx'move-to 90 130)
      (ctx'line-to 95 25)
      (ctx'line-to 150 80)
      (ctx'line-to 205 25)
      (ctx'line-to 210 130)
      (set! (~ ctx'line-width) 15)
      (ctx'stroke))))
