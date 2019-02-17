(use gauche.record)
(use graviton)
(use srfi-27)

(define-record-type ball
  %make-ball #t
  sprite
  (vx)
  (vy))

(define (make-ball win z)
  (let1 img (make-image 64 64)
    (draw-circle img
                 (center-point img)
                 32
                 (rgba (+ (random-integer #xff) 1)
                       (+ (random-integer #xff) 1)
                       (+ (random-integer #xff) 1)
                       (+ (random-integer #xff) 1))
                 :fill? #t)
    (%make-ball (make-sprite win :image img :z z :center (list (random-integer (floor->exact (border-right win)))
                                                               (random-integer (floor->exact (border-bottom win)))))
                (- (* (random-real) 400) 200)
                (- (* (random-real) 400) 200))))

(define (main args)
  (grv-begin
    (let* ((win (make-window (car args) 1024 768))
           (balls (map (^z (make-ball win z)) (iota 1000))))
      (on-update win ()
        (for-each (lambda (ball)
                    (let ((x (+ (point-x (sprite-center (ball-sprite ball))) (* (ball-vx ball) (frame-duration))))
                          (y (+ (point-y (sprite-center (ball-sprite ball))) (* (ball-vy ball) (frame-duration)))))
                      (cond
                        ((and (<= 0 x (border-right win))
                              (<= 0 y (border-bottom win)))
                         (set! (sprite-center (ball-sprite ball)) (list x y)))
                        (else
                         (unless (<= 0 x (border-right win))
                           (ball-vx-set! ball (- (ball-vx ball))))
                         (unless (<= 0 y (border-bottom win))
                           (ball-vy-set! ball (- (ball-vy ball))))))))
                  balls))
      (on-key-down win (scan key mod repeat?)
        (case scan
          ((escape)
           (destroy-window win))
          ((f)
           (set! (window-fullscreen? win) (not (window-fullscreen? win))))))
      )))
