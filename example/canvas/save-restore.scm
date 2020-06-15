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

    (save-context)

    (set-fill-style! "green")
    (fill-rect 10 10 100 100)

    (restore-context)

    (fill-rect 150 40 100 100)))
