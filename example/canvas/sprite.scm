(use gauche.logger)
(use gauche.parameter)
(use gauche.parseopt)
(use gauche.record)
(use gauche.threads)
(use gauche.time)
(use graviton)
(use math.const)
(use srfi-27)
(use srfi-42)

(define *sprite-width* 64)
(define *sprite-height* 64)
(define *num-patterns* 16)
(define *canvas-width* 1024)
(define *canvas-height* 768)
(define *tick* (/. 1 30))

(define-record-type ball
  #t #t
  pattern-id
  (x)
  (y)
  (vx)
  (vy))

(define (prepare-ball-images canvas ball-width ball-height num-balls)
  (parameterize ((current-canvas canvas))
    (dotimes (i num-balls)
      (begin-path)
      (set-fill-style! (format "#~3,'0X" (random-integer #xfff)))
      (ellipse (+ (* i ball-width) (/ ball-width 2))
               (/ ball-height 2)
               (/ ball-width 2)
               (/ ball-height 2)
               0
               0
               (* 2 pi))
      (fill))))

(define (update-balls! balls)
  (for-each (lambda (ball)
              (let ((x (+ (ball-x ball) (* (ball-vx ball) *tick*)))
                    (y (+ (ball-y ball) (* (ball-vy ball) *tick*))))
                (cond
                  ((and (<= 0 x (- *canvas-width* 1))
                        (<= 0 y (- *canvas-height* 1)))
                   (ball-x-set! ball x)
                   (ball-y-set! ball y))
                  ((not (<= 0 x (- *canvas-width* 1)))
                   (ball-vx-set! ball (- (ball-vx ball))))
                  ((not (<= 0 y (- *canvas-height* 1)))
                   (ball-vy-set! ball (- (ball-vy ball)))))))
            balls))

(define (draw-balls sprite balls)
  (set-fill-style! "#000")
  (fill-rect 0 0 *canvas-width* *canvas-height*)
  (for-each (lambda (ball)
              (draw-canvas sprite
                           (* (ball-pattern-id ball) *sprite-width*)
                           0
                           *sprite-width*
                           *sprite-height*
                           (round->exact (- (ball-x ball) (/. *sprite-width* 2)))
                           (round->exact (- (ball-y ball) (/. *sprite-height* 2)))
                           *sprite-width*
                           *sprite-height*))
            balls))

(define (main args)
  ;; (set-graviton-open-dev-tools! #t)
  ;; (set-graviton-use-player! #f)
  ;; (set-graviton-port! 8080)
  (set-graviton-background-color! "black")
  (let-args (cdr args) ((num-sprites "s|sprites=i" 100))
    (grv-begin
      (set-window-event-handler! 'keyup (lambda (event)
                                          (when (equal? (slot-ref event 'code) "Escape")
                                            (app-close))))
      (let ((sprite (make-canvas (* *sprite-width* *num-patterns*) *sprite-height* :visible? #f))
            (canvas (make-canvas *canvas-width* *canvas-height*))
            (balls (list-ec (: i num-sprites)
                            (make-ball (modulo i *num-patterns*)
                                       (random-integer *canvas-width*)
                                       (random-integer *canvas-height*)
                                       (- (* (random-real) 400) 200)
                                       (- (* (random-real) 400) 200)))))
        (prepare-ball-images sprite *sprite-width* *sprite-height* *num-patterns*)
        (loop-frame
          (lambda (break)
            (parameterize ((current-canvas canvas))
              (draw-balls sprite balls)
              (update-balls! balls)))))))
  0)
