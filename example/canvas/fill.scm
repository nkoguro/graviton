(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

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
    (fill 'evenodd)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
