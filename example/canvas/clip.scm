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
      (ctx'arc 100 75 50 0 (* 2 pi))
      (ctx'clip)
      (set! (~ ctx'fill-style) "blue")
      (ctx'fill-rect 0 0 (~ canvas'width) (~ canvas'height))
      (set! (~ ctx'fill-style) "orange")
      (ctx'fill-rect 0 0 100 100))))
