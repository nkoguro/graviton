(use graviton)
(use graviton.canvas)
(use math.const)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 300 150)

    (arc 0 0 5 0 (* 2 pi))
    (set-fill-style! "blue")
    (fill)

    (set-fill-style! "gray")
    (fill-rect 100 0 80 20)

    (rotate (* 45 pi/180))
    (set-fill-style! "red")
    (fill-rect 100 0 80 20)

    (set-transform! 1 0 0 1 0 0)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
