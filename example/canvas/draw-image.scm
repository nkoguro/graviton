(use file.util)
(use graviton)
(use graviton.grut)

(bind-url-path "/font_16x16.png" (build-path (sys-dirname (current-load-path)) "../font_16x16.png"))

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 300)
  :theme 'dark)

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let1 image (load-image "/font_16x16.png")
      (ctx'draw-image image 0 0))))
