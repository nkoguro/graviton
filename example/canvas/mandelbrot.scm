;;
;; Compute Mandelbrot set.
;;

(use data.queue)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use util.combinations)
(use util.match)

(define (mandelbrot-fill channel width height offset-width offset-height start-x start-y delta-x delta-y max-loop time-slice)
  (define (x+y->z x y)
    (make-rectangular (+ start-x (* x delta-x)) (+ start-y (* y delta-y))))

  (define (compute zn c n)
    (cond ((>= n max-loop) n)
          ((>= (magnitude zn) 2) n)
          (else (compute (+ (* zn zn) c) c (+ n 1)))))

  (do ((y offset-height (+ y 1)))
      ((<= (+ height offset-height) y) #f)
    (do ((x offset-width (+ x 1)))
        ((<= (+ width offset-width) x) #f)
      (let1 n (compute 0 (x+y->z x y) 0)
        (when (< n max-loop)
          (channel-send channel (list x y n))))
      (when time-slice
        (task-yield time-slice)))))

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
    (task-queue-max-num-threads-set! (async-task-queue) 4)

    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (make-canvas *width* *height*)
    (let1 channel (make-channel)
      (let* ((delta-x (/. 3 *width*))
             (delta-y (/. 3 *height*))
             (mesh-x 16)
             (mesh-y 16)
             (mesh-w (round->exact (/. *width* mesh-x)))
             (mesh-h (round->exact (/. *height* mesh-y))))
        (for-each (match-lambda
                    ((i j)
                     (async
                       (mandelbrot-fill channel
                                        mesh-w
                                        mesh-h
                                        (* mesh-w i)
                                        (* mesh-h j)
                                        -2
                                        -1.5
                                        delta-x
                                        delta-y
                                        *max-n*
                                        #f))))
                  (cartesian-product (list (iota mesh-x) (iota mesh-y)))))

      (set-command-buffering? #t)
      (while (channel-recv/await channel)
        => point+n
        (match-let1 (x y n) point+n
          (set-fill-style! (n->color n *max-n*))
          (fill-rect x y 1 1)
          (task-yield (/. 1 10)))))))
