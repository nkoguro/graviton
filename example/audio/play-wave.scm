(use graviton)
(use graviton.audio)

(define (main args)
  (grv-player)

  (grv-begin
    (play-wave 0 'square 2000 0.1)
    (play-wave 0 'square 1000 0.1)
    (asleep 1.0)
    (app-close))
  0)
