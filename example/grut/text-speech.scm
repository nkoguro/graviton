(use gauche.parseopt)
(use graviton)
(use graviton.grut)

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))

    (with-window (grut-text-window :padding "5px" :scrollbar? #t)
        (text-console)
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (let1 voice (query-voice :default #t)
        (format text-console "Type ~atext message. Press ESC to exit.\n" (if voice #"~(~ voice'lang) " ""))
        (when voice
          (format text-console "\"~a\" voice will be used.\n" (~ voice'name)))
        (newline text-console)

        (while #t
          (let1 msg (read-text/edit text-console :prompt ">")
            (unless (= (string-length msg) 0)
              (format text-console "Speaking \"~a\".~%" msg)
              (speak msg))))))))
