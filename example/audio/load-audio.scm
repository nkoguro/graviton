(use file.util)
(use gauche.logger)
(use graviton)

(current-directory (sys-dirname (current-load-path)))

(define (main args)
  (grv-player :show? #f)

  (grv-begin
    (let1 audio (load-audio "pipo.mp3")
      (log-format "duration: ~a sec" (~ audio'duration))
      (audio'play)
      (worker-sleep! 0.5))
    (grv-exit)))
