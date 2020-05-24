(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use graviton.event)

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (make-canvas 300 150)
    (rect 10 10 100 100)
    (stroke)
    (log-format "In stroke: ~a" (await (is-point-in-stroke? 50 10))))
  0)
