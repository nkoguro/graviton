(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (let1 canvas (make-canvas 300 150)
      (rect 10 10 100 100)
      (fill)

      (let1 image (get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (await (download-image-data image))))
        (put-image-data image 150 10)))))
