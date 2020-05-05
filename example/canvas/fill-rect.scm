(use graviton)
(use graviton.canvas)

(define (main args)
  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (make-canvas 400 200)
    (set-fill-style! "green")
    (fill-rect 10 10 100 100))
  0)
