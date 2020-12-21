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
      (ctx'begin-path)
      (ctx'move-to 30 90)
      (ctx'line-to 110 20)
      (ctx'line-to 240 130)
      (ctx'line-to 60 130)
      (ctx'line-to 190 20)
      (ctx'line-to 270 90)
      (ctx'close-path)
      (set! (~ ctx'fill-style) "green")
      (ctx'fill "evenodd"))))
