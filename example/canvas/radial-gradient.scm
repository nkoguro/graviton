(use graviton2)

(define (main args)
  (grv-begin
    (make-canvas 200 200)
    (set-fill-style! (radial-gradient 110 90 30 100 100 70 '((0 "pink") (0.9 "white") (1 "green"))))
    (fill-rect 20 20 160 160))
  0)