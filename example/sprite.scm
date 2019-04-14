(use gauche.hook)
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
                 (center-x img)
                 (center-y img)
                 32
                 (color 'white)
                 :fill? #t)
    img))

(define (make-ball win img z)
  (%make-ball (make-sprite win
                           :image img
                           :x (random-integer (window-logical-width win))
                           :y (random-integer (window-logical-height win))
                           :z z
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
      (add-hook! (window-update-begin-hook-of win)
        (lambda (win)
          (for-each (lambda (ball)
                      (let ((x (+ (sprite-x (ball-sprite ball)) (* (ball-vx ball) (/ 1 (frame-per-second)))))
                            (y (+ (sprite-y (ball-sprite ball)) (* (ball-vy ball) (/ 1 (frame-per-second))))))
                        (cond
                          ((and (<= 0 x (- (window-logical-width win) 1))
                                (<= 0 y (- (window-logical-height win) 1)))
                           (set-sprite-x! (ball-sprite ball) x)
                           (set-sprite-y! (ball-sprite ball) y))
                          (else
                           (unless (<= 0 x (- (window-logical-width win) 1))
                             (ball-vx-set! ball (- (ball-vx ball))))
                           (unless (<= 0 y (- (window-logical-height win) 1))
                             (ball-vy-set! ball (- (ball-vy ball))))))))
                    balls)))
      (add-hook! (key-down-hook-of win)
        (lambda (win scan key mod repeat?)
          (case scan
            ((escape)
             (destroy-window win))
            ((f)
             (set! (window-fullscreen? win) (not (window-fullscreen? win)))))))
      ))
  0)
