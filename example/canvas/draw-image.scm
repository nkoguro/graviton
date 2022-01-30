(use file.util)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)

(bind-url-path "/font_16x16.png" (build-path (sys-dirname (current-load-path)) "../font_16x16.png"))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f))
    (grv-config :client (cond
                          (force-player? 'player)
                          (force-browser? 'browser)
                          (else #f)))
    (with-window (grut-canvas-window 256 256 :background-color "black")
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (let1 image (load-image "/font_16x16.png")
          (ctx'draw-image image 0 0))))))
