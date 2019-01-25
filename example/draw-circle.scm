(use graviton)
(use math.const)

(define (deg x)
  (* (/ x 180.0) pi))

(define (main args)
  (let ((orbit-img (create-image 710 710))
        (core-img (create-image 52 52)))
    (draw-circle orbit-img (center-point orbit-img) 350 (color 'teal) :radius-ratio 0.3 :fill? #t)
    (draw-circle orbit-img (center-point orbit-img) 340 0 :radius-ratio 0.28 :fill? #t)
    (draw-circle core-img (center-point core-img) 25 (color 'teal) :fill? #t)
    (call-with-window *program-name* '(1024 768)
      (lambda (win)
        (put-image win orbit-img (center-x win) (center-y win) :angle 30)
        (put-image win orbit-img (center-x win) (center-y win) :angle 90)
        (put-image win orbit-img (center-x win) (center-y win) :angle 150)
        (put-image win core-img (center-x win) (center-y win)))))
  0)
