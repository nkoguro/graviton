(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player :show? #f)

  (grv-begin
    (let1 text "Hello world"
      (format #t "The text width of '~a' is ~a px" text (~ (ctx'measure-text text) 'width)))

    (grv-exit)))
