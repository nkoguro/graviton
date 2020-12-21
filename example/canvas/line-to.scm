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
      (ctx'move-to 90 130)
      (ctx'line-to 95 25)
      (ctx'line-to 150 80)
      (ctx'line-to 205 25)
      (ctx'line-to 210 130)
      (set! (~ ctx'line-width) 15)
      (ctx'stroke))))
