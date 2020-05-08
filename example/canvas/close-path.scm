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
    (begin-path)
    (move-to 20 140)
    (line-to 120 10)
    (line-to 220 140)
    (close-path)
    (stroke))
  0)
