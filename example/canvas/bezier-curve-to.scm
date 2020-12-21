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
      ;; Cubic Bezier curve
      (ctx'begin-path)
      (ctx'move-to 50 20)
      (ctx'bezier-curve-to 230 30 150 80 250 100)
      (ctx'stroke)

      ;; Start and end points
      (set! (~ ctx'fill-style) "blue")
      (ctx'begin-path)
      (ctx'arc 50 20 5 0 (* 2 pi))
      (ctx'arc 250 100 5 0 (* 2 pi))
      (ctx'fill)

      ;; Control points
      (set! (~ ctx'fill-style) "red")
      (ctx'begin-path)
      (ctx'arc 230 30 5 0 (* 2 pi))
      (ctx'arc 150 80 5 0 (* 2 pi))
      (ctx'fill))))
