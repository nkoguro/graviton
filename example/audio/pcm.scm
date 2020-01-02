(use gauche.logger)
(use graviton2)

(define (main args)
  (grv-begin
    (let1 pcm (await (load-pcm "example/audio/pipo.mp3"))
      (log-format "PCM duration: ~a sec" (slot-ref pcm 'duration))
      (play-pcm 0 pcm 0 1.0)
      (play-pcm 0 pcm 1200 1.0))
    (asleep 1.0)
    (app-close)
    )
  0)
