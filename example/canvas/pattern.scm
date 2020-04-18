(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (let1 pat (load-canvas "example/canvas/Canvas_createpattern.png" :visible? #f)
      (make-canvas 300 300)
      (set-fill-style! (pattern (await pat)))
      (fill-rect 0 0 300 300)))
  0)
