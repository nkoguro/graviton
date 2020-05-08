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
    (move-to 30 90)
    (line-to 110 20)
    (line-to 240 130)
    (line-to 60 130)
    (line-to 190 20)
    (line-to 270 90)
    (close-path)
    (set-fill-style! "green")
    (fill 'evenodd))
  0)
