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

    (ctx'begin-path)
    (ctx'move-to 20 140)
    (ctx'line-to 120 10)
    (ctx'line-to 220 140)
    (ctx'close-path)
    (ctx'stroke)))
