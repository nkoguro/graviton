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
    (begin-path)
    (move-to 20 140)
    (line-to 120 10)
    (line-to 220 140)
    (close-path)
    (stroke)))
