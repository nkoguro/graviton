(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use math.const)

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f))
    (grv-config :client (cond
                          (force-player? 'player)
                          (force-browser? 'browser)
                          (else #f)))
    (with-window (grut-canvas-window 300 150)
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (ctx'begin-path)
        (ctx'arc 100 75 50 0 (* 2 pi))
        (ctx'clip)
        (set! (~ ctx'fill-style) "blue")
        (ctx'fill-rect 0 0 (~ canvas'width) (~ canvas'height))
        (set! (~ ctx'fill-style) "orange")
        (ctx'fill-rect 0 0 100 100)))))
