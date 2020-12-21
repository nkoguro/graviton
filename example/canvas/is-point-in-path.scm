(use gauche.logger)
(use graviton)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body :style "background-color: gray"
     (html:canvas :width 300 :height 150 :class "grv-object-fit-contain" :style "background-color: white")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (ctx'rect 10 10 100 100)
      (ctx'fill)

      (on-jsevent canvas "click" (offset-x offset-y)
        (log-format "(~a, ~a) in path?: ~a" offset-x offset-y (ctx'is-point-in-path offset-x offset-y))))))
