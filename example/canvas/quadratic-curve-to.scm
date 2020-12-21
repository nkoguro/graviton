(use graviton)
(use math.const)
(use text.html-lite)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 300 :height 150 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))
      (ctx'begin-path)
      (ctx'move-to 50 20)
      (ctx'quadratic-curve-to 230 30 50 100)
      (ctx'stroke)

      (set! (~ ctx'fill-style) "blue")
      (ctx'begin-path)
      (ctx'arc 50 20 5 0 (* 2 pi))
      (ctx'arc 50 100 5 0 (* 2 pi))
      (ctx'fill)

      (set! (~ ctx'fill-style) "red")
      (ctx'begin-path)
      (ctx'arc 230 30 5 0 (* 2 pi))
      (ctx'fill))))
