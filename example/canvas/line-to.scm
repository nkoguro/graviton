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

      (ctx'move-to 90 130)
      (ctx'line-to 95 25)
      (ctx'line-to 150 80)
      (ctx'line-to 205 25)
      (ctx'line-to 210 130)
      (set! (~ ctx'line-width) 15)
      (ctx'stroke))))

(define (main args)
  (grv-start-player))
