(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'rect 10 10 100 100)
    (ctx'fill)

    (let1 image (ctx'get-image-data 60 60 200 100)
      (log-format "image-data content length: ~a" (u8vector-length (~ image'data)))
      (ctx'put-image-data image 150 10))))
