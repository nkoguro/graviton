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
           (ctx (canvas'get-context "2d"))
           (gradient (ctx'create-linear-gradient 0 0 200 0)))
      (gradient'add-color-stop 0 "green")
      (gradient'add-color-stop 0.7 "white")
      (gradient'add-color-stop 1 "pink")
      (set! (~ ctx'fill-style) gradient)
      (ctx'fill-rect 10 10 200 100))))
