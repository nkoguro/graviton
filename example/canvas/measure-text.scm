(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window
                 :body (html:body
                        (html:canvas :id "canvas" :width 300 :height 150))
                 :show? #f)
      (canvas)
    (let ((ctx (canvas'get-context "2d"))
          (text "Hello world"))
      (format #t "The text width of '~a' is ~a px" text (~ (ctx'measure-text text) 'width))
      (close-window))))
