(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (let1 canvas (make-canvas 300 150)
      (set-fill-style! (linear-gradient 20 0 220 0 '((0 "green") (0.5 "cyan") (1 "green"))))
      (fill-rect 20 20 200 100))

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
