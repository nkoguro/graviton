(use graviton)
(use graviton.canvas)
(use math.const)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (make-canvas 300 450)
    (begin-path)
    (ellipse 150 150 75 112 pi/4 0 (* 2 pi))
    (stroke)

    (begin-path)
    (set-line-dash! #(5 5))
    (move-to 0 300)
    (line-to 300 0)
    (stroke)

    (set-fill-style! "red")
    (begin-path)
    (ellipse 60 375 50 30 (* pi 0.25) 0 (* pi 1.5))
    (fill)

    (set-fill-style! "blue")
    (begin-path)
    (ellipse 150 375 50 30 (* pi 0.25) 0 pi)
    (fill)

    (set-fill-style! "green")
    (begin-path)
    (ellipse 240 375 50 30 (* pi 0.25) 0 pi #t)
    (fill)

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
