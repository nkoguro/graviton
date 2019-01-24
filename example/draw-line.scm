(use graviton)

(define (main args)
  (call-with-window *program-name* 'fullscreen
    (lambda (win)
      (set-window-resolution! win 320 240)
      (let* ((step 20)
             (dx (/ 320 step))
             (dy (/ 240 step)))
        (dotimes (i step)
          (draw-line win
                     `(,(center-point win)
                       (,(* i dx) ,(border-top win)))
                     (color 'white))
          (draw-line win
                     `(,(center-point win)
                       (,(border-right win) ,(* i dy)))
                     (color 'yellow))
          (draw-line win
                     `(,(center-point win)
                       (,(- (border-right win) (* i dx)) ,(border-bottom win)))
                     (color 'blue))
          (draw-line win
                     `(,(center-point win) (,(border-left win) ,(- (border-bottom win) (* i dy))))
                     (color 'fuchsia))))))
  0)