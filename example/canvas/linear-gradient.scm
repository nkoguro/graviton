(use graviton2)

(define (main args)
  (grv-begin
    (let1 canvas (make-canvas 300 150)
      (set-fill-style! (linear-gradient 20 0 220 0 '((0 "green") (0.5 "cyan") (1 "green"))))
      (fill-rect 20 20 200 100)))
  0)
