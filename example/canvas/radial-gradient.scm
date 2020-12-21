(use graviton)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 200 :height 200 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d"))
           (gradient (ctx'create-radial-gradient 110 90 30 100 100 70)))
      (gradient'add-color-stop 0 "pink")
      (gradient'add-color-stop 0.9 "white")
      (gradient'add-color-stop 1 "green")
      (set! (~ ctx'fill-style) gradient)
      (ctx'fill-rect 20 20 160 160))))
