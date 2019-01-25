(use graviton)
(use math.const)
(use srfi-42)

(define (main args)
  (let1 img (create-image 640 480)
    (set-coordinate! img -1.2 1.2 1.2 -1.2)
    (let1 d (* 2 (/ (* 2 pi) 5))
      (draw-polygon img
                    (map (^p `(,(cos p) ,(sin p)))
                         (list-ec
                           (:range i 5)
                           (+ pi/2 (* d i))))
                    (color 'green)
                    :fill? #t)
      (draw-polygon img
                    (map (^p `(,(cos p) ,(sin p)))
                         (list-ec
                           (:range i 5)
                           (+ pi/2 (* d i))))
                    (color 'white)))
    (display-image img))
  0)

