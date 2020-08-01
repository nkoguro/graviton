(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 200 200)
    (set-fill-style! (radial-gradient 110 90 30 100 100 70 '((0 "pink") (0.9 "white") (1 "green"))))
    (fill-rect 20 20 160 160)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
