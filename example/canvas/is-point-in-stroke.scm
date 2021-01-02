(use gauche.logger)
(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :id canvas :context-2d ctx :width 300 :height 150 :background-color "white")
  :background-color "gray")

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (ctx'rect 10 10 100 100)
    (ctx'stroke)

    (on-jsevent canvas "click" (offset-x offset-y)
      (log-format "(~a, ~a) in stroke?: ~a" offset-x offset-y (ctx'is-point-in-stroke offset-x offset-y)))))
