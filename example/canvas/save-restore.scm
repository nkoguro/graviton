(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (make-canvas-window 300 150)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (ctx'save)

      (set! (~ ctx'fill-style) "green")
      (ctx'fill-rect 10 10 100 100)

      (ctx'restore)

      (ctx'fill-rect 150 40 100 100))))
