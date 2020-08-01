(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 300 150)
    (rect 10 10 100 100)
    (stroke)
    (log-format "In stroke: ~a" (force (is-point-in-stroke? 50 10)))

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
