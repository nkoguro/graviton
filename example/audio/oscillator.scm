(use graviton2)

(define (main args)
  (grv-begin
    (play-sound 0 'square 2000 0.1)
    (play-sound 0 'square 1000 0.1)
    (asleep 0.5)
    (app-close))
  0)
