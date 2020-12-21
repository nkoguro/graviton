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
      (ctx'arc 0 0 5 0 (* 2 pi))
      (set! (~ ctx'fill-style) "blue")
      (ctx'fill)

      (set! (~ ctx'fill-style) "gray")
      (ctx'fill-rect 100 0 80 20)

      (ctx'rotate (* 45 pi/180))
      (set! (~ ctx'fill-style) "red")
      (ctx'fill-rect 100 0 80 20)

      (ctx'set-transform 1 0 0 1 0 0))))
