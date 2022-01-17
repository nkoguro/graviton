(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (grut-text-window)
      (text)
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (close-window)))

    (let1 voice (query-voice :default #t)
      (format text "Type ~atext message. Press ESC to exit.\n" (if voice #"~(~ voice'lang) " ""))
      (when voice
        (format text "\"~a\" voice will be used.\n" (~ voice'name)))
      (newline text)

      (while #t
        (let1 msg (read-text/edit text :prompt ">")
          (unless (= (string-length msg) 0)
            (format text "Speaking \"~a\".~%" msg)
            (speak msg)))))))
