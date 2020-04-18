(use graviton)
(use graviton.canvas)

(define-syntax close-stroke-begin
  (syntax-rules ()
    ((_ expr ...)
     (begin
     (begin-path)
     expr ...
     (close-path)
     (stroke)))))

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (make-canvas 400 200)
    (close-stroke-begin
      (move-to 20 20)
      (line-to 200 20)
      (line-to 120 120))
    (clear-rect 10 10 100 100))
  0)
