(use graviton)
(use graviton.canvas)
(use math.const)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

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
      (fill)

      (port-for-each (match-lambda
                       (('keyup _ "Escape")
                        (event-stream-close))
                       (_
                        #f))
                     next-event)

      0)))
