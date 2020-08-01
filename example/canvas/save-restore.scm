(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 300 150)

    (save-context)

    (set-fill-style! "green")
    (fill-rect 10 10 100 100)

    (restore-context)

    (fill-rect 150 40 100 100)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
