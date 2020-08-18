(use file.util)
(use graviton)
(use graviton.canvas)
(use util.match)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (let1 pat (load-image (build-path *program-dir* "Canvas_createpattern.png") :visible? #f)
      (make-canvas 300 300)
      (set-fill-style! (pattern (force pat)))
      (fill-rect 0 0 300 300))

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
