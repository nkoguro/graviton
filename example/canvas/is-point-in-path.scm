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
    (rect 10 10 100 100)
    (fill)
    (log-format "In path: ~a" (await (is-point-in-path? 30 70)))))
