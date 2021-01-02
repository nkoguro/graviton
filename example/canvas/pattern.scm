(use file.util)
(use graviton)
(use graviton.grut)

(bind-url-path "/" (sys-dirname (current-load-path)))

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 300))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let1 pat (ctx'create-pattern (load-image "/Canvas_createpattern.png") "repeat")
      (set! (~ ctx'fill-style) pat)
      (ctx'fill-rect 0 0 300 300))))
