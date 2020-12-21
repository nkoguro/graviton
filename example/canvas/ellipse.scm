(use graviton)
(use math.const)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 300 :height 450 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (ctx'begin-path)
      (ctx'ellipse 150 150 75 112 pi/4 0 (* 2 pi))
      (ctx'stroke)

      (ctx'begin-path)
      (ctx'set-line-dash #(5 5))
      (ctx'move-to 0 300)
      (ctx'line-to 300 0)
      (ctx'stroke)

      (set! (~ ctx'fill-style) "red")
      (ctx'begin-path)
      (ctx'ellipse 60 375 50 30 (* pi 0.25) 0 (* pi 1.5))
      (ctx'fill)

      (set! (~ ctx'fill-style) "blue")
      (ctx'begin-path)
      (ctx'ellipse 150 375 50 30 (* pi 0.25) 0 pi)
      (ctx'fill)

      (set! (~ ctx'fill-style) "green")
      (ctx'begin-path)
      (ctx'ellipse 240 375 50 30 (* pi 0.25) 0 pi #t)
      (ctx'fill))))
