(use graviton)

(define (main args)
  (call-with-window *program-name* '(640 480)
    (lambda (win)
      (set-coordinate! win -1.2 1.2 1.2 -1.2)
      (draw-rect win '(1.0 0.7) '(-1.0 -0.7) (color 'aqua))
      (draw-rect win '(0.5 0.3) '(-0.5 -0.2) (color 'fuchsia) :fill? #t)
      (draw-rect win '(0.5 0.3) '(-0.5 -0.2) (color 'white))))
  0)

