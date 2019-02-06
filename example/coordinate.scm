(use graviton)

(define (main args)
  (let1 img (make-image 640 480)
    (set-border! img 4 6)
    (draw-line img `((,(border-left img) 0) (,(border-right img) 0)) (color 'aqua))
    (draw-line img `((0 ,(border-top img)) (0 ,(border-bottom img))) (color 'aqua))
    (draw-line img
               (map (^x (make-point x (* 2 (sin x))))
                    (iota (+ (* (- (border-right img) (border-left img)) 10) 1) (border-left img) 0.1))
               (color 'white))
    (display-image img))
  0)