(use graviton2)
(use math.const)

(define (main args)
  (grv-begin
    (make-canvas 300 150)

    (begin-path)
    (move-to 50 20)
    (quadratic-curve-to 230 30 50 100)
    (stroke)

    (set-fill-style! "blue")
    (begin-path)
    (arc 50 20 5 0 (* 2 pi))
    (arc 50 100 5 0 (* 2 pi))
    (fill)

    (set-fill-style! "red")
    (begin-path)
    (arc 230 30 5 0 (* 2 pi))
    (fill))
  0)