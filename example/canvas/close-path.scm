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

      (ctx'begin-path)
      (ctx'move-to 20 140)
      (ctx'line-to 120 10)
      (ctx'line-to 220 140)
      (ctx'close-path)
      (ctx'stroke))))

(define (main args)
  (grv-start-player))
