(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (make-canvas 400 200)
    (set-fill-style! "green")
    (fill-rect 10 10 100 100))
  0)
