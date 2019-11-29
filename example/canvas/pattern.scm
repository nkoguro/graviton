(use graviton2)

(define (main args)
  (set-graviton-open-dev-tools! #f)
  (grv-begin
    (let1 pat (load-canvas "example/canvas/Canvas_createpattern.png" :visible? #f)
      (make-canvas 300 300)
      (set-fill-style! (pattern (await pat)))
      (fill-rect 0 0 300 300)))
  0)
