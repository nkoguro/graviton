(use graviton)
(use graviton.canvas)
(use math.const)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (let1 canvas (make-canvas 300 150)
      (begin-path)
      (arc 100 75 50 0 (* 2 pi))
      (clip)
      (set-fill-style! "blue")
      (fill-rect 0 0 (slot-ref canvas 'width) (slot-ref canvas 'height))
      (set-fill-style! "orange")
      (fill-rect 0 0 100 100)

      (port-for-each (match-lambda
                       (('keyup _ "Escape")
                        (event-stream-close))
                       (_
                        #f))
                     next-event)

      0)))
