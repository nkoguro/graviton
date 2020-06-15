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
      ;; Cubic Bezier curve
      (begin-path)
      (move-to 50 20)
      (bezier-curve-to 230 30 150 80 250 100)
      (stroke)

      ;; Start and end points
      (set-fill-style! "blue")
      (begin-path)
      (arc 50 20 5 0 (* 2 pi))
      (arc 250 100 5 0 (* 2 pi))
      (fill)

      ;; Control points
      (set-fill-style! "red")
      (begin-path)
      (arc 230 30 5 0 (* 2 pi))
      (arc 150 80 5 0 (* 2 pi))
      (fill))))
