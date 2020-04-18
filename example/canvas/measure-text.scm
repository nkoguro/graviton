(use gauche.logger)
(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (make-canvas 300 150)
    (log-format "text width: ~a" (slot-ref (await (measure-text "Hello world")) 'width)))
  0)
