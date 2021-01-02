(use file.util)
(use gauche.logger)
(use graviton)
(use graviton.grut)

(bind-url-path "/pipo.mp3" (build-path (sys-dirname (current-load-path)) "pipo.mp3"))

(define (main args)
  (grv-player :show? #f)

  (grv-begin
    (let1 audio (load-audio "/pipo.mp3")
      (log-format "duration: ~a sec" (~ audio'duration))
      (audio'play)
      (worker-sleep! 0.5))
    (grv-exit)))
