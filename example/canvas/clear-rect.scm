(use graviton)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 400 :height 200 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (ctx'begin-path)
      (ctx'move-to 20 20)
      (ctx'line-to 200 20)
      (ctx'line-to 120 120)
      (ctx'close-path)
      (ctx'stroke)
      (ctx'clear-rect 10 10 100 100))))
