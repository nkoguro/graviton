(use file.util)
(use gauche.logger)
(use gauche.threads)
(use graviton)
(use graviton.audio)

(current-directory (sys-dirname (current-load-path)))

(define (main args)
  (grv-player)

  (grv-begin
    (let1 audio (force (load-audio "pipo.mp3"))
      (log-format "duration: ~a" (slot-ref audio 'duration))
      (play-audio audio)
      (thread-sleep! 0.5))
    0))
