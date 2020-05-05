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
    (set-font! "48px serif")
    (fill-text "Hello world" 50 100))
  0)
