(use file.util)
(use gauche.uvector)
(use graviton)
(use text.html-lite)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     :style "background-color: black"
     (html:canvas :width 300 :height 300 :class "grv-object-fit-contain")))

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d"))
           (image (load-image (build-path *program-dir* "../font_16x16.png"))))
      (ctx'draw-image image 0 0))))
