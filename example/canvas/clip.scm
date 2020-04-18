(use graviton)
(use graviton.canvas)
(use math.const)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (let1 canvas (make-canvas 300 150)
      (begin-path)
      (arc 100 75 50 0 (* 2 pi))
      (clip)
      (set-fill-style! "blue")
      (fill-rect 0 0 (slot-ref canvas 'width) (slot-ref canvas 'height))
      (set-fill-style! "orange")
      (fill-rect 0 0 100 100)))
  0)
