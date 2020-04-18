(use graviton)
(use graviton.canvas)
(use math.const)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
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
      (fill)))
  0)
