(use gauche.logger)
(use gauche.parameter)
(use gauche.parseopt)
(use gauche.record)
(use gauche.threads)
(use gauche.time)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)
(use srfi-27)
(use srfi-42)

(define *sprite-width* 64)
(define *sprite-height* 64)
(define *num-patterns* 16)
(define *canvas-width* 1024)
(define *canvas-height* 768)
(define *tick* (/. 1 30))

(define-macro (trace name)
  (let ((args (gensym))
        (start-time (gensym))
        (end-time (gensym))
        (filename (format "~a.perf.csv" name))
        (out (gensym))
        (proc (global-variable-ref (current-module) name)))
    (sys-unlink filename)
    `(define ,name (lambda ,args
                     (profiler-start)
                     (unwind-protect
                         (let ((,start-time (time->seconds (current-time))))
                           (apply ,proc ,args)
                           (let ((,end-time (time->seconds (current-time))))
                             (call-with-output-file ,filename
                               (lambda (,out)
                                 (format ,out "~a,~a~%" ,start-time (- ,end-time ,start-time)))
                               :if-exists :append)))
                       (profiler-stop))))))

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

(define (update-frame canvas sprite balls)
  (draw-balls sprite balls)
  (update-balls! balls))

;; (trace update-frame)
;; (trace draw-balls)
;; (trace update-balls!)

(define (main args)
  (grv-player :background-color "black")

  (let-args (cdr args) ((num-sprites "s|sprites=i" 100))
    (grv-begin
      (add-event-listener! (client-window) "keyup"
                           '("key")
        (lambda (key)
          (when (equal? key "Escape")
            (client-close))))

      (let ((sprite (make-canvas (* *sprite-width* *num-patterns*) *sprite-height* :visible? #f))
            (canvas (make-canvas *canvas-width* *canvas-height*))
            (balls (list-ec (: i num-sprites)
                            (make-ball (modulo i *num-patterns*)
                                       (random-integer *canvas-width*)
                                       (random-integer *canvas-height*)
                                       (- (* (random-real) 400) 200)
                                       (- (* (random-real) 400) 200)))))
        (prepare-ball-images sprite *sprite-width* *sprite-height* *num-patterns*)
        (parameterize ((current-canvas canvas))
          (loop-frame
            (lambda (break)
              (update-frame canvas sprite balls))))))))
