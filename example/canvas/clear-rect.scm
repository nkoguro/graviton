(use graviton)
(use graviton.grut)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :class "grut-contain" :width 400 :height 200))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (ctx'begin-path)
      (ctx'move-to 20 20)
      (ctx'line-to 200 20)
      (ctx'line-to 120 120)
      (ctx'close-path)
      (ctx'stroke)
      (ctx'clear-rect 10 10 100 100))))

(define (main args)
  (grv-start-player))
