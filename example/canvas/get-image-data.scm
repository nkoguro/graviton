(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use util.match)

(define (main args)
  (grv-player)

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (let1 canvas (make-canvas 300 150)
      (rect 10 10 100 100)
      (fill)

      (let1 image (get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (force (download-image-data image))))
        (put-image-data image 150 10)))

    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
