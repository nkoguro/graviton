(use file.util)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)

(define image-path (build-path (sys-dirname (current-load-path)) "Canvas_createpattern.png"))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))
    (with-window (grut-canvas-window 300 300)
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (let1 pat (ctx'create-pattern (load-image (file->url image-path)) "repeat")
          (set! (~ ctx'fill-style) pat)
          (ctx'fill-rect 0 0 300 300))))))
