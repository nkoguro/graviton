(use gauche.logger)
(use graviton)
(use graviton.grut)
(use text.html-lite)

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
    (ctx'fill)

    (on-jsevent canvas "click" (offset-x offset-y)
      (log-format "(~a, ~a) in path?: ~a" offset-x offset-y (ctx'is-point-in-path offset-x offset-y)))))
