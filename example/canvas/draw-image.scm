(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use math.const)

(define (main args)
  (set-graviton-background-color! "#000")
  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (let ((canvas (make-canvas 300 300))
          (loaded-image (load-canvas "example/font_16x16.png" :visible? #f)))
      (draw-canvas (await loaded-image) 0 0)))
  0)
