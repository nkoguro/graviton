(use gauche.parseopt)
(use graviton)
(use graviton.grut)

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
    (with-window (grut-canvas-window 300 150 :background-color "white")
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (let1 gradient (ctx'create-linear-gradient 0 0 200 0)
          (gradient'add-color-stop 0 "green")
          (gradient'add-color-stop 0.7 "white")
          (gradient'add-color-stop 1 "pink")
          (set! (~ ctx'fill-style) gradient)
          (ctx'fill-rect 10 10 200 100))))))
