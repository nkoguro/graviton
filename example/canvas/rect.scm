(use graviton)
(use graviton.canvas)
(use graviton.event)

(define (main args)
  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (make-canvas 300 150)
    (rect 10 20 150 100)
    (fill))
  0)
