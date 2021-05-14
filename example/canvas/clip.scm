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

      (ctx'begin-path)
      (ctx'arc 100 75 50 0 (* 2 pi))
      (ctx'clip)
      (set! (~ ctx'fill-style) "blue")
      (ctx'fill-rect 0 0 (~ canvas'width) (~ canvas'height))
      (set! (~ ctx'fill-style) "orange")
      (ctx'fill-rect 0 0 100 100))))

(define (main args)
  (grv-start-player))
