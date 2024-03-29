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
    (with-window (grut-canvas-window 300 150 :background-color "white")
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (ctx'arc 0 0 5 0 (* 2 pi))
        (set! (~ ctx'fill-style) "blue")
        (ctx'fill)

        (set! (~ ctx'fill-style) "gray")
        (ctx'fill-rect 100 0 80 20)

        (ctx'rotate (* 45 pi/180))
        (set! (~ ctx'fill-style) "red")
        (ctx'fill-rect 100 0 80 20)

        (ctx'set-transform 1 0 0 1 0 0)))))
