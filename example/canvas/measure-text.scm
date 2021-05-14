(use graviton)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :width 300 :height 150))

  (let-elements (canvas)
    (let ((ctx (canvas'get-context "2d"))
          (text "Hello world"))
      (format #t "The text width of '~a' is ~a px" text (~ (ctx'measure-text text) 'width))
      (grv-exit))))

(define (main args)
  (grv-start-player :show? #f))
