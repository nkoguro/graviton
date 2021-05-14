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

      (let1 gradient (ctx'create-linear-gradient 0 0 200 0)
        (gradient'add-color-stop 0 "green")
        (gradient'add-color-stop 0.7 "white")
        (gradient'add-color-stop 1 "pink")
        (set! (~ ctx'fill-style) gradient)
        (ctx'fill-rect 10 10 200 100)))))

(define (main args)
  (grv-start-player))
