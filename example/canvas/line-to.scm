(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'move-to 90 130)
    (ctx'line-to 95 25)
    (ctx'line-to 150 80)
    (ctx'line-to 205 25)
    (ctx'line-to 210 130)
    (set! (~ ctx'line-width) 15)
    (ctx'stroke)))
