(use gauche.record)
(use graviton)
(use srfi-27)

(define-record-type ball
  %make-ball #t
  sprite
  (vx)
  (vy))

(define (ball-image)
  (let1 img (make-image 64 64)
    (draw-circle img
                 (center-point img)
                 32
                 (color 'white)
                 :fill? #t)
    img))

(define (make-ball win img z)
  (%make-ball (make-sprite win
                           :image img
                           :z z
                           :center (list (random-integer (window-logical-width win))
                                         (random-integer (window-logical-height win)))
                           :color (rgba (+ (random-integer #xff) 1)
                                        (+ (random-integer #xff) 1)
                                        (+ (random-integer #xff) 1)
                                        (+ (random-integer #xff) 1)))
              (- (* (random-real) 400) 200)
              (- (* (random-real) 400) 200)))

(define (main args)
  (grv-begin
    (let* ((win (make-window (car args) 1024 768))
           (img (ball-image))
           (balls (map (^z (make-ball win img z)) (iota 1000))))
      (on-update win ()
        (for-each (lambda (ball)
                    (let ((x (+ (point-x (sprite-center (ball-sprite ball))) (* (ball-vx ball) (frame-duration))))
                          (y (+ (point-y (sprite-center (ball-sprite ball))) (* (ball-vy ball) (frame-duration)))))
                      (cond
                        ((and (<= 0 x (- (window-logical-width win) 1))
                              (<= 0 y (- (window-logical-height win) 1)))
                         (set! (sprite-center (ball-sprite ball)) (list x y)))
                        (else
                         (unless (<= 0 x (- (window-logical-width win) 1))
                           (ball-vx-set! ball (- (ball-vx ball))))
                         (unless (<= 0 y (- (window-logical-height win) 1))
                           (ball-vy-set! ball (- (ball-vy ball))))))))
                  balls))
      (on-key-down win (scan key mod repeat?)
        (case scan
          ((escape)
           (destroy-window win))
          ((f)
           (set! (window-fullscreen? win) (not (window-fullscreen? win))))))
      )))
