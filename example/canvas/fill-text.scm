(use graviton2)

(define (main args)
  (grv-begin
    (make-canvas 400 200)
    (set-font! "48px serif")
    (fill-text "Hello world" 50 100))
  0)
