(use gauche.logger)
(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (make-canvas 300 150)
    (rect 10 10 100 100)
    (stroke)
    (log-format "In stroke: ~a" (await (is-point-in-stroke? 50 10))))
  0)
