(use graviton)
(use graviton.canvas)
(use util.match)

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
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 400 200)
    (close-stroke-begin
      (move-to 20 20)
      (line-to 200 20)
      (line-to 120 120))
    (clear-rect 10 10 100 100)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)
    0))
