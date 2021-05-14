(use file.util)
(use graviton)
(use graviton.grut)
(use text.html-lite)

(bind-url-path "/" (sys-dirname (current-load-path)))

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :class "grut-contain" :width 300 :height 300))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

      (let1 pat (ctx'create-pattern (load-image "/Canvas_createpattern.png") "repeat")
        (set! (~ ctx'fill-style) pat)
        (ctx'fill-rect 0 0 300 300)))))

(define (main args)
  (grv-start-player))
