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
    (fill)
    (log-format "In path: ~a" (await (is-point-in-path? 30 70))))
  0)
