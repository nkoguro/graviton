(use graviton2)

(define (main args)
  (grv-begin
    (make-canvas 300 150)
    (move-to 90 130)
    (line-to 95 25)
    (line-to 150 80)
    (line-to 205 25)
    (line-to 210 130)
    (set-line-width! 15)
    (stroke))
  0)