(use graviton)
(use graviton.canvas)
(use graviton.event)

(define-syntax close-stroke-begin
  (syntax-rules ()
    ((_ expr ...)
     (begin
     (begin-path)
     expr ...
     (close-path)
     (stroke)))))

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (make-canvas 400 200)
    (close-stroke-begin
      (move-to 20 20)
      (line-to 200 20)
      (line-to 120 120))
    (clear-rect 10 10 100 100)))
