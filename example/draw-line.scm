(use graviton)

(define (main args)
  (let1 img (create-image 320 240)
    (let* ((step 20)
           (dx (/ 320 step))
           (dy (/ 240 step)))
      (dotimes (i step)
        (draw-line img
                   `(,(center-point img)
                     (,(* i dx) ,(border-top img)))
                   (color 'white))
        (draw-line img
                   `(,(center-point img)
                     (,(border-right img) ,(* i dy)))
                   (color 'yellow))
        (draw-line img
                   `(,(center-point img)
                     (,(- (border-right img) (* i dx)) ,(border-bottom img)))
                   (color 'blue))
        (draw-line img
                   `(,(center-point img) (,(border-left img) ,(- (border-bottom img) (* i dy))))
                   (color 'fuchsia))))
    (display-image img :fullscreen? #t))
  0)