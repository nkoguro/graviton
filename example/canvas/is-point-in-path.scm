(use gauche.logger)
(use graviton)

(define (main args)
  (grv-begin
    (make-canvas 300 150)
    (rect 10 10 100 100)
    (fill)
    (log-format "In path: ~a" (await (is-point-in-path? 30 70))))
  0)
