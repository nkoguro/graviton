(use gauche.logger)
(use gauche.uvector)
(use graviton)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 300 :height 150 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (ctx'rect 10 10 100 100)
      (ctx'fill)

      (let1 image (ctx'get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (~ image'data)))
        (ctx'put-image-data image 150 10)))))
