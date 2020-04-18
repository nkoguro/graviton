(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (let1 canvas (make-canvas 300 150)
      (set-fill-style! (linear-gradient 20 0 220 0 '((0 "green") (0.5 "cyan") (1 "green"))))
      (fill-rect 20 20 200 100)))
  0)
