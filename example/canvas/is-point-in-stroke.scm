(use gauche.logger)
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
    (with-window (grut-canvas-window 300 150)
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (ctx'rect 10 10 100 100)
        (ctx'stroke)

        (on-jsevent canvas "click" (offset-x offset-y)
          (log-format "(~a, ~a) in stroke?: ~a" offset-x offset-y (ctx'is-point-in-stroke offset-x offset-y)))))))
