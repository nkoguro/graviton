(use gauche.parseopt)
(use gauche.record)
(use graviton)
(use graviton.misc)
(use math.const)
(use srfi-27)
(use srfi-42)
(use text.html-lite)

(define *sprite-width* 64)
(define *sprite-height* 64)
(define *num-patterns* 16)
(define *canvas-width* 1024)
(define *canvas-height* 768)

(define *num-samples* #f)

(define-record-type ball
  #t #t
  pattern-id
  (x)
  (y)
  (vx)
  (vy))

(define (prepare-ball-images canvas ball-width ball-height num-balls)
  (let1 ctx (canvas'get-context "2d")
    (dotimes (i num-balls)
      (ctx'begin-path)
      (set! (~ ctx 'fill-style) (format "#~3,'0X" (random-integer #xfff)))
      (ctx'ellipse (+ (* i ball-width) (/ ball-width 2))
                   (/ ball-height 2)
                   (/ ball-width 2)
                   (/ ball-height 2)
                   0
                   0
                   (* 2 pi))
      (ctx'fill))))

(define (update-balls! balls tick)
  (for-each (lambda (ball)
              (let ((x (+ (ball-x ball) (* (ball-vx ball) tick)))
                    (y (+ (ball-y ball) (* (ball-vy ball) tick))))
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

(define (draw-balls canvas sprite balls)
  (let1 ctx (canvas'get-context "2d")
    (ctx'clear-rect 0 0 *canvas-width* *canvas-height*)
    (stat-memory #f
                 *num-samples*
                 (for-each (lambda (ball)
                             (ctx'draw-image sprite
                                             (* (ball-pattern-id ball) *sprite-width*)
                                             0
                                             *sprite-width*
                                             *sprite-height*
                                             (round->exact (- (ball-x ball) (/. *sprite-width* 2)))
                                             (round->exact (- (ball-y ball) (/. *sprite-height* 2)))
                                             *sprite-width*
                                             *sprite-height*))
                           balls))))

(define (update-frame canvas sprite balls tick)
  (update-balls! balls tick)
  (draw-balls canvas sprite balls))

(define (main args)
  (let-args (cdr args) ((num-sprites "s|sprites=i" 100)
                        (use-browser? "b|browser" #f)
                        (num-samples "num-samples=i" #f))
    (set! *num-samples* num-samples)

    (if use-browser?
      (grv-browser)
      (grv-player))

    (define-document-content
      (html:body :style "background-color:black; margin:0"
                 (html:canvas :id "screen"
                              :width *canvas-width*
                              :height *canvas-height*
                              :class "grv-object-fit-contain")))

    (grv-begin
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit 0)))

      (let ((sprite (document'create-element "canvas"))
            (canvas (document'query-selector "#screen"))
            (balls (list-ec (: i num-sprites)
                            (make-ball (modulo i *num-patterns*)
                                       (random-integer *canvas-width*)
                                       (random-integer *canvas-height*)
                                       (- (* (random-real) 400) 200)
                                       (- (* (random-real) 400) 200)))))
        (set! (~ sprite'width) (* *sprite-width* *num-patterns*))
        (set! (~ sprite'height) *sprite-height*)
        (prepare-ball-images sprite *sprite-width* *sprite-height* *num-patterns*)

        (on-repaint (sec-per-frame)
          (stat (:description "frame per second" :unit "fps" :format-spec "~,2f" :order-by :desc)
                *num-samples*
                (/. 1 sec-per-frame))
          (stat-time "repaint"
                     *num-samples*
                     (update-frame canvas sprite balls sec-per-frame)))))))
