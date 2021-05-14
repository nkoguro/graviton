(use graviton)
(use graviton.grut)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :class "grut-contain" :width 300 :height 150))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (ctx'save)

      (set! (~ ctx'fill-style) "green")
      (ctx'fill-rect 10 10 100 100)

      (ctx'restore)

      (ctx'fill-rect 150 40 100 100))))

(define (main args)
  (grv-start-player))
