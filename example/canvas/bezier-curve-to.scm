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

        ;; Cubic Bezier curve
        (ctx'begin-path)
        (ctx'move-to 50 20)
        (ctx'bezier-curve-to 230 30 150 80 250 100)
        (ctx'stroke)

        ;; Start and end points
        (set! (~ ctx'fill-style) "blue")
        (ctx'begin-path)
        (ctx'arc 50 20 5 0 (* 2 pi))
        (ctx'arc 250 100 5 0 (* 2 pi))
        (ctx'fill)

        ;; Control points
        (set! (~ ctx'fill-style) "red")
        (ctx'begin-path)
        (ctx'arc 230 30 5 0 (* 2 pi))
        (ctx'arc 150 80 5 0 (* 2 pi))
        (ctx'fill)))))
