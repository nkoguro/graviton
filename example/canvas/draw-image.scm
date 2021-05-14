(use file.util)
(use graviton)
(use graviton.grut)
(use text.html-lite)

(bind-url-path "/font_16x16.png" (build-path (sys-dirname (current-load-path)) "../font_16x16.png"))

(grv-window
  :path "/"
  :body
  (html:body :style "background-color: black"
   (html:canvas :id "canvas" :class "grut-contain" :width 300 :height 300))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (let1 image (load-image "/font_16x16.png")
        (ctx'draw-image image 0 0)))))

(define (main args)
  (grv-start-player))
