(use graviton)
(use graviton.grut)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :class "grut-contain" :width 200 :height 200))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (let1 gradient (ctx'create-radial-gradient 110 90 30 100 100 70)
        (gradient'add-color-stop 0 "pink")
        (gradient'add-color-stop 0.9 "white")
        (gradient'add-color-stop 1 "green")
        (set! (~ ctx'fill-style) gradient)
        (ctx'fill-rect 20 20 160 160)))))

(define (main args)
  (grv-start-player))
