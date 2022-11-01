(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use math.const)

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
    (with-window (grut-canvas-window 300 450 :background-color "white")
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (ctx'begin-path)
        (ctx'ellipse 150 150 75 112 pi/4 0 (* 2 pi))
        (ctx'stroke)

        (ctx'begin-path)
        (ctx'set-line-dash #(5 5))
        (ctx'move-to 0 300)
        (ctx'line-to 300 0)
        (ctx'stroke)

        (set! (~ ctx'fill-style) "red")
        (ctx'begin-path)
        (ctx'ellipse 60 375 50 30 (* pi 0.25) 0 (* pi 1.5))
        (ctx'fill)

        (set! (~ ctx'fill-style) "blue")
        (ctx'begin-path)
        (ctx'ellipse 150 375 50 30 (* pi 0.25) 0 pi)
        (ctx'fill)

        (set! (~ ctx'fill-style) "green")
        (ctx'begin-path)
        (ctx'ellipse 240 375 50 30 (* pi 0.25) 0 pi #t)
        (ctx'fill)))))
