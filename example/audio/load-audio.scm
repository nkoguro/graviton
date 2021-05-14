(use file.util)
(use gauche.logger)
(use graviton)
(use graviton.grut)

(bind-url-path "/pipo.mp3" (build-path (sys-dirname (current-load-path)) "pipo.mp3"))

(grv-window
  :path "/"

  (let1 audio (load-audio "/pipo.mp3")
      (log-format "duration: ~a sec" (~ audio'duration))
      (audio'play)
      (worker-sleep! 0.5))
  (grv-exit))

(define (main args)
  (grv-start-player :show? #f))
