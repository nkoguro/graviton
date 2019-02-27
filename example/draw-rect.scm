(use graviton)

(define (main args)
  (let1 img (make-image 640 480)
    (set-border! img 1.2)
    (draw-rect img -1.0 -0.7 2.0 1.4 (color 'aqua))
    (draw-rect img -0.5 -0.2 1.0 0.5 (color 'fuchsia) :fill? #t)
    (draw-rect img -0.5 -0.2 1.0 0.5 (color 'white))
    (display-image img))
  0)

