(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use graviton.event)

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (make-canvas 300 150)
    (log-format "text width: ~a" (slot-ref (await (measure-text "Hello world")) 'width))))
