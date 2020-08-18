(use file.util)
(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use util.match)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player :background-color "#000")

  (grv-begin
    (capture-jsevent (client-window) "keyup" '("key"))

    (let ((canvas (make-canvas 300 300))
          (loaded-image (load-image (build-path *program-dir* "../font_16x16.png") :visible? #f)))
      (draw-image (force loaded-image) 0 0))


    (port-for-each (match-lambda
                     (('keyup _ "Escape")
                      (event-stream-close))
                     (_
                      #f))
                   next-event)

    0))
