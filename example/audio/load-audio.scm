(use file.util)
(use gauche.logger)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)

(bind-url-path "/pipo.mp3" (build-path (sys-dirname (current-load-path)) "pipo.mp3"))

(define (is-browser?)
    (eq? (grv-config-parameter 'client) 'browser))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f))
    (grv-config :client (cond
                          (force-player? 'player)
                          (force-browser? 'browser)
                          (else #f)))

    (with-window (cond
                   ((is-browser?)
                    (grut-text-window :font-size 24 :padding 5))
                   (else
                    #f))
        (text-console)
      (parameterize ((current-output-port (or text-console (current-output-port))))
        (when (is-browser?)
          (display "Hit space key to play.\n")
          (while (not (equal? (jsevent-await window "keyup" '(key)) " "))
            #t))

        (let1 audio (load-audio "/pipo.mp3")
          (log-format "duration: ~a sec" (~ audio'duration))
          (audio'play)
          (asleep 0.5))

        (when (is-browser?)
          (display "done."))
        (close-window)))))
