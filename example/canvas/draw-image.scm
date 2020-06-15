(use file.util)
(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player :background-color "#000")

  (grv-begin
    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (let ((canvas (make-canvas 300 300))
          (loaded-image (load-canvas (build-path *program-dir* "../font_16x16.png") :visible? #f)))
      (draw-canvas (await loaded-image) 0 0))))
