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

    (ctx'save)

    (set! (~ ctx'fill-style) "green")
    (ctx'fill-rect 10 10 100 100)

    (ctx'restore)

    (ctx'fill-rect 150 40 100 100)))
