(use file.util)
(use graviton)
(use text.html-lite)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 300 :height :300 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d"))
           (pat (ctx'create-pattern (load-image (build-path *program-dir* "Canvas_createpattern.png")) "repeat")))
      (set! (~ ctx'fill-style) pat)
      (ctx'fill-rect 0 0 300 300))))
