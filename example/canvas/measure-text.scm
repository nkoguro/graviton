(use gauche.logger)
(use graviton2)

(define (main args)
  (grv-begin
    (make-canvas 300 150)
    (log-format "text width: ~a" (slot-ref (await (measure-text "Hello world")) 'width)))
  0)
