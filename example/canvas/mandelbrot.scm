;;
;; Compute Mandelbrot set.
;;

(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use util.combinations)
(use util.match)

(define *width* 500)
(define *height* 500)
(define *max-n* 50)

(define (n->color n max-n)
  (let ((h (round->exact (* (/. (- max-n n) max-n) 360)))
        (l (round->exact (* (/. n max-n) 100))))
    (format "hsl(~a,100%,~a%)" h l)))

(define compute-worker
  (grv-worker
   :name "mandelbrot compute worker"
   :size 4
   (define-message compute (width height offset-width offset-height start-x start-y delta-x delta-y max-loop)
     (define (x+y->z x y)
       (make-rectangular (+ start-x (* x delta-x)) (+ start-y (* y delta-y))))

     (define (compute zn c n)
       (cond ((>= n max-loop) n)
             ((>= (magnitude zn) 2) n)
             (else (compute (+ (* zn zn) c) c (+ n 1)))))

     (let1 pixels '()
       (do ((y offset-height (+ y 1)))
           ((<= (+ height offset-height) y) #f)
         (do ((x offset-width (+ x 1)))
             ((<= (+ width offset-width) x) #f)
           (let1 n (compute 0 (x+y->z x y) 0)
             (when (< n max-loop)
               (push! pixels (list x y (n->color n *max-n*)))))))
       ((main-worker)'draw-tile pixels)))))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))
    (with-window (grut-canvas-window *width* *height* :background-color "black")
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (define-message draw-tile (pixels)
          :priority 'low
          (fold (lambda (pixel prev-color)
                  (match-let1 (x y color) pixel
                    (unless (equal? prev-color color)
                      (set! (~ ctx'fill-style) color))
                    (ctx'fill-rect x y 1 1)
                    color))
                #f
                pixels))

        (let* ((delta-x (/. 3 *width*))
               (delta-y (/. 3 *height*))
               (mesh-x 16)
               (mesh-y 16)
               (mesh-w (round->exact (/. *width* mesh-x)))
               (mesh-h (round->exact (/. *height* mesh-y))))
          (for-each (match-lambda
                      ((i j)
                       (compute-worker'compute mesh-w
                                               mesh-h
                                               (* mesh-w i)
                                               (* mesh-h j)
                                               -2
                                               -1.5
                                               delta-x
                                               delta-y
                                               *max-n*)))
                    (cartesian-product (list (iota mesh-x) (iota mesh-y)))))))))
