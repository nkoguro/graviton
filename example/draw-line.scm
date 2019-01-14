(use graviton)

(define (main args)
  (call-with-window *program-name* 'fullscreen
    (lambda (win)
      (set-window-resolution! win 640 480)
      (let1 step 10
        (dotimes (x (/ 640 step))
          (draw-line win 320 240 (* x step) 0 #xffffffff))
        (dotimes (y (/ 480 step))
          (draw-line win 320 240 639 (* y step) #xff00ffff))
        (dotimes (x (/ 640 step))
          (draw-line win 320 240 (- 639 (* x step)) 479 #xff0000ff))
        (dotimes (y (/ 480 step))
          (draw-line win 320 240 0 (- 479 (* y step)) #xffff00ff)))))
  0)