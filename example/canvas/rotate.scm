(use graviton2)
(use math.const)

(define (main args)
  (grv-begin
    (make-canvas 300 150)

    (arc 0 0 5 0 (* 2 pi))
    (set-fill-style! "blue")
    (fill)

    (set-fill-style! "gray")
    (fill-rect 100 0 80 20)

    (rotate (* 45 pi/180))
    (set-fill-style! "red")
    (fill-rect 100 0 80 20)

    (set-transform! 1 0 0 1 0 0))
  0)
