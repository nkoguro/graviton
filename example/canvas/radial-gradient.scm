(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (make-canvas 200 200)
    (set-fill-style! (radial-gradient 110 90 30 100 100 70 '((0 "pink") (0.9 "white") (1 "green"))))
    (fill-rect 20 20 160 160))
  0)
