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
    (ctx'move-to 30 90)
    (ctx'line-to 110 20)
    (ctx'line-to 240 130)
    (ctx'line-to 60 130)
    (ctx'line-to 190 20)
    (ctx'line-to 270 90)
    (ctx'close-path)
    (set! (~ ctx'fill-style) "green")
    (ctx'fill "evenodd")))
