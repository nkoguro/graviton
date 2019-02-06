(use graviton)
(use math.const)

(define (deg x)
  (* (/ x 180.0) pi))

(define (main args)
  (let1 img (make-image 1024 768)
    (draw-circle img (center-point img) 350 (color 'teal) :radius-ratio 0.3 :thickness 10 :rotate (deg 30))
    (draw-circle img (center-point img) 350 (color 'teal) :radius-ratio 0.3 :thickness 10 :rotate (deg 90))
    (draw-circle img (center-point img) 350 (color 'teal) :radius-ratio 0.3 :thickness 10 :rotate (deg 150))
    (draw-circle img (center-point img) 25 (color 'teal) :fill? #t)
    (display-image img))
  0)
