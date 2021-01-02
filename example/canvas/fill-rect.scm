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

    (set! (~ ctx'fill-style) "green")
    (ctx'fill-rect 10 10 100 100)))
