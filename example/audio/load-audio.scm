(use gauche.logger)
(use graviton)
(use graviton.audio)

(define (main args)
  (grv-begin
    (let1 audio (await (load-audio "example/audio/pipo.mp3"))
      (log-format "duration: ~a" (slot-ref audio 'duration))
      (play-audio audio)
      (asleep 0.5))
    (app-close))
  0)
