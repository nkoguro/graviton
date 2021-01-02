(use graviton)
(use graviton.grut)
(use math.const)

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'begin-path)
    (ctx'move-to 50 20)
    (ctx'quadratic-curve-to 230 30 50 100)
    (ctx'stroke)

    (set! (~ ctx'fill-style) "blue")
    (ctx'begin-path)
    (ctx'arc 50 20 5 0 (* 2 pi))
    (ctx'arc 50 100 5 0 (* 2 pi))
    (ctx'fill)

    (set! (~ ctx'fill-style) "red")
    (ctx'begin-path)
    (ctx'arc 230 30 5 0 (* 2 pi))
    (ctx'fill)))
