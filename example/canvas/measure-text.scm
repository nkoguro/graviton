(use graviton)
(use text.html-lite)

(define (main args)
  (grv-player :show? #f)

  (define-document-content
    (html:body
     (html:canvas :width 300 :height 150 :class "grv-object-fit-contain")))

  (grv-begin
    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d"))
           (text "Hello world"))
      (format #t "The text width of '~a' is ~a px" text (~ (ctx'measure-text "Hello world") 'width)))

    (grv-exit)))
