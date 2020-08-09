;;
;; Compute Mandelbrot set.
;;

(use control.thread-pool)
(use data.queue)
(use gauche.generator)
(use gauche.parameter)
(use graviton)
(use graviton.canvas)
(use util.combinations)
(use util.match)

(define (mandelbrot-fill width height offset-width offset-height start-x start-y delta-x delta-y max-loop)
  (define (x+y->z x y)
    (make-rectangular (+ start-x (* x delta-x)) (+ start-y (* y delta-y))))

  (define (compute zn c n)
    (cond ((>= n max-loop) n)
          ((>= (magnitude zn) 2) n)
          (else (compute (+ (* zn zn) c) c (+ n 1)))))

  (do ((y offset-height (+ y 1))
       (tile-data '()))
      ((<= (+ height offset-height) y) (fire-event 'tile (list tile-data) 10))
    (do ((x offset-width (+ x 1)))
        ((<= (+ width offset-width) x) #f)
      (let1 n (compute 0 (x+y->z x y) 0)
        (when (< n max-loop)
          (push! tile-data (list x y n)))))))

(define (n->color n max-n)
  (let ((h (round->exact (* (/. (- max-n n) max-n) 360)))
        (l (round->exact (* (/. n max-n) 100))))
    (format "hsl(~a,100%,~a%)" h l)))

(define *width* 500)
(define *height* 500)
(define *max-n* 50)

(define (main args)
  (grv-player :background-color "black")

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas *width* *height*)
    (let ((pool (make-thread-pool 4)))
      (let* ((delta-x (/. 3 *width*))
             (delta-y (/. 3 *height*))
             (mesh-x 16)
             (mesh-y 16)
             (mesh-w (round->exact (/. *width* mesh-x)))
             (mesh-h (round->exact (/. *height* mesh-y))))
        (parameterize ((current-pool pool))
          (for-each (match-lambda
                      ((i j)
                       (async
                         (mandelbrot-fill mesh-w
                                          mesh-h
                                          (* mesh-w i)
                                          (* mesh-h j)
                                          -2
                                          -1.5
                                          delta-x
                                          delta-y
                                          *max-n*))))
                    (cartesian-product (list (iota mesh-x) (iota mesh-y))))))

      (do-generator (event next-event)
        (match event
          (('keyup _ "Escape")
           (event-stream-close))
          (('tile tile-data)
           (with-jstransaction
             (lambda ()
               (let1 prev-color #f
                 (for-each (match-lambda ((x y n)
                                          (let1 color (n->color n *max-n*)
                                            (unless (equal? prev-color color)
                                              (set-fill-style! (n->color n *max-n*)))
                                            (set! prev-color color))
                                          (fill-rect x y 1 1)))
                           tile-data)))))
          (_
           #f)))

      0)))
