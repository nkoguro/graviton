(use graviton)

(define (main args)
  (call-with-window *program-name* 'fullscreen
    (lambda (win)
      (set-window-resolution! win 320 240)
      (let1 step 10
        (dotimes (x (/ 320 step))
          (draw-line win `((160 120) (,(* x step) 0)) (color 'white)))
        (dotimes (y (/ 240 step))
          (draw-line win `((160 120) (319 ,(* y step))) (color 'yellow)))
        (dotimes (x (/ 320 step))
          (draw-line win `((160 120) (,(- 319 (* x step)) 239)) (color 'blue)))
        (dotimes (y (/ 240 step))
          (draw-line win `((160 120) (0 ,(- 239 (* y step)))) (color 'fuchsia))))))
  0)