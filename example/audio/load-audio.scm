(use file.util)
(use gauche.logger)
(use graviton)
(use graviton.grut)

(bind-url-path "/pipo.mp3" (build-path (sys-dirname (current-load-path)) "pipo.mp3"))

(define (main args)
  (with-window #f
      ()
    (let1 audio (load-audio "/pipo.mp3")
      (log-format "duration: ~a sec" (~ audio'duration))
      (audio'play)
      (grv-sleep! 0.5))
    (close-window)))
