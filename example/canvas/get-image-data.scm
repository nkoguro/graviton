(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (make-canvas-window 300 150)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (ctx'rect 10 10 100 100)
      (ctx'fill)

      (let1 image (ctx'get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (~ image'data)))
        (ctx'put-image-data image 150 10)))))
