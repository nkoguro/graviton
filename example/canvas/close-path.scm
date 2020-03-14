(use graviton)

(define (main args)
  (grv-begin
    (make-canvas 300 150)
    (begin-path)
    (move-to 20 140)
    (line-to 120 10)
    (line-to 220 140)
    (close-path)
    (stroke))
  0)
