(use gauche.logger)
(use graviton)
(use graviton.grut)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body :style "background-color: gray"
             (html:canvas :id "canvas" :class "grut-contain" :width 300 :height 150 :style "background-color: white"))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (ctx'rect 10 10 100 100)
      (ctx'stroke)

      (on-jsevent canvas "click" (offset-x offset-y)
        (log-format "(~a, ~a) in stroke?: ~a" offset-x offset-y (ctx'is-point-in-stroke offset-x offset-y))))))

(define (main args)
  (grv-start-player))
