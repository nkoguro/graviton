(use gauche.logger)
(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (make-canvas 300 150)
    (log-format "text width: ~a" (slot-ref (await (measure-text "Hello world")) 'width)))
  0)
