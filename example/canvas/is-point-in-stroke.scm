(use gauche.logger)
(use graviton)

(define (main args)
  (grv-begin
    (make-canvas 300 150)
    (rect 10 10 100 100)
    (stroke)
    (log-format "In stroke: ~a" (await (is-point-in-stroke? 50 10))))
  0)
