(use gauche.logger)
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
    (with-window (grut-canvas-window 150 200)
        (canvas)
      (let1 ctx (canvas'get-context "2d")
        (log-format "window width=~a, height=~a" (~ window'inner-width) (~ window'inner-height))

        (on-jsevent window "keyup" (key)
          (when (equal? key "Escape")
            (close-window)))

        (on-jsevent canvas "click" (offset-x offset-y)
          (log-format "mouse: x=~a, y=~a" offset-x offset-y))

        (dotimes (i 4)
          (dotimes (j 3)
            (let ((x (+ 25 (* j 50)))
                  (y (+ 25 (* i 50)))
                  (radius 20)
                  (start-angle 0)
                  (end-angle (+ pi (/ (* pi j) 2)))
                  (anti-clockwise (odd? (modulo i 2))))
              (ctx'begin-path)
              (ctx'arc x y radius start-angle end-angle anti-clockwise)
              (if (< 1 i)
                (ctx'fill)
                (ctx'stroke)))))))))
