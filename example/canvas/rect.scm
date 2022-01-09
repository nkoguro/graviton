(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (grut-canvas-window 300 150)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (ctx'rect 10 20 150 100)
      (ctx'fill))))
