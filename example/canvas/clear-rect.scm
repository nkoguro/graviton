(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :context-2d ctx :width 400 :height 200))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'begin-path)
    (ctx'move-to 20 20)
    (ctx'line-to 200 20)
    (ctx'line-to 120 120)
    (ctx'close-path)
    (ctx'stroke)
    (ctx'clear-rect 10 10 100 100)))
