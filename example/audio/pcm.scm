(use gauche.logger)
(use graviton)

(define (main args)
  (grv-begin
    (let1 pcm (await (load-pcm "example/audio/pipo.mp3"))
      (log-format "PCM duration: ~a sec" (slot-ref pcm 'duration))
      (play-pcm 0 pcm 0 1.0 #f #f 0.07)
      (play-pcm 0 pcm 1200 1.0 `(0 ,(slot-ref pcm 'duration)) (slot-ref pcm 'duration)))
    (asleep 1.0)
    (app-close)
    )
  0)
