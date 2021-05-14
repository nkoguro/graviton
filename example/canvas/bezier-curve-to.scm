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

      ;; Cubic Bezier curve
      (ctx'begin-path)
      (ctx'move-to 50 20)
      (ctx'bezier-curve-to 230 30 150 80 250 100)
      (ctx'stroke)

      ;; Start and end points
      (set! (~ ctx'fill-style) "blue")
      (ctx'begin-path)
      (ctx'arc 50 20 5 0 (* 2 pi))
      (ctx'arc 250 100 5 0 (* 2 pi))
      (ctx'fill)

      ;; Control points
      (set! (~ ctx'fill-style) "red")
      (ctx'begin-path)
      (ctx'arc 230 30 5 0 (* 2 pi))
      (ctx'arc 150 80 5 0 (* 2 pi))
      (ctx'fill))))

(define (main args)
  (grv-start-player))
