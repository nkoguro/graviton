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
      (ctx'move-to 30 90)
      (ctx'line-to 110 20)
      (ctx'line-to 240 130)
      (ctx'line-to 60 130)
      (ctx'line-to 190 20)
      (ctx'line-to 270 90)
      (ctx'close-path)
      (set! (~ ctx'fill-style) "green")
      (ctx'fill "evenodd"))))

(define (main args)
  (grv-start-player))
