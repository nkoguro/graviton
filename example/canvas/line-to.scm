(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 300 150)
    (move-to 90 130)
    (line-to 95 25)
    (line-to 150 80)
    (line-to 205 25)
    (line-to 210 130)
    (set-line-width! 15)
    (stroke)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
