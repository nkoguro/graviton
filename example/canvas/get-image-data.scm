(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use graviton.canvas)
(use math.const)

(define (main args)
  (grv-begin
    (set-window-event-handler! 'keyup (lambda (event)
                                        (when (equal? (slot-ref event 'code) "Escape")
                                          (app-close))))
    (let1 canvas (make-canvas 300 150)
      (rect 10 10 100 100)
      (fill)

      (let1 image (get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (await (download-image-data image))))
        (put-image-data image 150 10))))
  0)
