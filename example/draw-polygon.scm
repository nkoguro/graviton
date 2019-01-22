(use graviton)
(use math.const)
(use srfi-42)

(define (main args)
  (call-with-window *program-name* 'fullscreen
    (lambda (win)
      (set-coordinate! win -1.2 1.2 1.2 -1.2)
      (let1 d (* 2 (/ (* 2 pi) 5))
        (draw-polygon win
                      (map (^p `(,(cos p) ,(sin p)))
                           (list-ec
                             (:range i 5)
                             (+ pi/2 (* d i))))
                      (color 'green)
                      :fill? #t)
        (draw-polygon win
                      (map (^p `(,(cos p) ,(sin p)))
                           (list-ec
                             (:range i 5)
                             (+ pi/2 (* d i))))
                      (color 'white)))))
  0)

