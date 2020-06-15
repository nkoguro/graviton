(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (let1 canvas (make-canvas 300 150)
      ;; Tangential lines
      (begin-path)
      (set-stroke-style! "gray")
      (move-to 200 20)
      (line-to 200 130)
      (line-to 50 20)
      (stroke)

      ;; Arc
      (begin-path)
      (set-stroke-style! "black")
      (set-line-width! 5)
      (move-to 200 20)
      (arc-to 200 130 50 20 40)
      (stroke)

      ;; Start point
      (begin-path)
      (set-fill-style! "blue")
      (arc 200 20 5 0 (* 2 pi))
      (fill)

      ;; Control points
      (begin-path)
      (set-fill-style! "red")
      (arc 200 130 5 0 (* 2 pi))
      (arc 50 20 5 0 (* 2 pi))
      (fill))))
