;;
;; Compute Mandelbrot set.
;;

(use graviton)
(use text.html-lite)
(use util.combinations)
(use util.match)

(define *width* 500)
(define *height* 500)
(define *max-n* 50)

(define (n->color n max-n)
  (let ((h (round->exact (* (/. (- max-n n) max-n) 360)))
        (l (round->exact (* (/. n max-n) 100))))
    (format "hsl(~a,100%,~a%)" h l)))

(define (compute-worker-main)
  (on-event 'compute (width height offset-width offset-height start-x start-y delta-x delta-y max-loop)
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
      (worker-fire-event (main-worker) 'draw-tile pixels))))

(define (main args)
  (grv-player)

  (define-document-content
    (html:body :style "background-color: black"
               (html:canvas :width *width* :height *height* :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (on-event ('draw-tile :priority 'low) (pixels)
        (fold (lambda (pixel prev-color)
                (match-let1 (x y color) pixel
                  (unless (equal? prev-color color)
                    (set! (~ ctx'fill-style) color))
                  (ctx'fill-rect x y 1 1)
                  color))
              #f
              pixels)))

    (let* ((worker (run-worker-thread compute-worker-main :name "mandelbrot compute worker" :size 4))
           (delta-x (/. 3 *width*))
           (delta-y (/. 3 *height*))
           (mesh-x 16)
           (mesh-y 16)
           (mesh-w (round->exact (/. *width* mesh-x)))
           (mesh-h (round->exact (/. *height* mesh-y))))
      (for-each (match-lambda
                  ((i j)
                   (worker-fire-event worker
                                      'compute
                                      mesh-w
                                      mesh-h
                                      (* mesh-w i)
                                      (* mesh-h j)
                                      -2
                                      -1.5
                                      delta-x
                                      delta-y
                                      *max-n*)))
                (cartesian-product (list (iota mesh-x) (iota mesh-y)))))))
