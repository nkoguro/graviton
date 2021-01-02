(use graviton)
(use graviton.grut)
(use math.const)

(define-grut-window
  (canvas :id canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'begin-path)
    (ctx'arc 100 75 50 0 (* 2 pi))
    (ctx'clip)
    (set! (~ ctx'fill-style) "blue")
    (ctx'fill-rect 0 0 (~ canvas'width) (~ canvas'height))
    (set! (~ ctx'fill-style) "orange")
    (ctx'fill-rect 0 0 100 100)))
