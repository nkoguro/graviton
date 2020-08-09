(use gauche.threads)
(use graviton)
(use graviton.audio)

(define (main args)
  (grv-player :show? #f)

  (grv-begin
    (play-wave 0 'square 2000 0.1)
    (play-wave 0 'square 1000 0.1)
    (thread-sleep! 1.0)
    0))
