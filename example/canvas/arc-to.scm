(use graviton)
(use graviton.grut)
(use math.const)
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

      ;; Tangential lines
      (ctx'begin-path)
      (set! (~ ctx'stroke-style) "gray")
      (ctx'move-to 200 20)
      (ctx'line-to 200 130)
      (ctx'line-to 50 20)
      (ctx'stroke)

      ;; Arc
      (ctx'begin-path)
      (set! (~ ctx'stroke-style) "black")
      (set! (~ ctx'line-width) 5)
      (ctx'move-to 200 20)
      (ctx'arc-to 200 130 50 20 40)
      (ctx'stroke)

      ;; Start point
      (ctx'begin-path)
      (set! (~ ctx'fill-style) "blue")
      (ctx'arc 200 20 5 0 (* 2 pi))
      (ctx'fill)

      ;; Control points
      (ctx'begin-path)
      (set! (~ ctx'fill-style) "red")
      (ctx'arc 200 130 5 0 (* 2 pi))
      (ctx'arc 50 20 5 0 (* 2 pi))
      (ctx'fill))))

(define (main args)
  (grv-start-player))
