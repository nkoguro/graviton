(use file.util)
(use graviton)
(use graviton.grut)

(bind-url-path "/font_16x16.png" (build-path (sys-dirname (current-load-path)) "../font_16x16.png"))

(define (main args)
  (with-window (make-canvas-window 256 256 :background-color "black")
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (let1 image (load-image "/font_16x16.png")
        (ctx'draw-image image 0 0)))))
