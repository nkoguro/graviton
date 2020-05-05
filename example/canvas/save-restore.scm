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

    (save-context)

    (set-fill-style! "green")
    (fill-rect 10 10 100 100)

    (restore-context)

    (fill-rect 150 40 100 100))
  0)
