(use graviton)
(use graviton.grut)
(use srfi-42)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   :style "background-color: black; color: white"
   (html:grv-text :id "text" :class "grut-monospace-font grut-contain" :data-column 42 :data-row 15))

  (let-elements (text)
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (with-output-to-port text
      (lambda ()
        (display "System colors:\n")
        (do-ec (: color 8)
               (display #"\x1b[48;5;~|color|m "))
        (display "\x1b[0m\n")
        (do-ec (: color 8 16)
               (display #"\x1b[48;5;~|color|m "))
        (display "\x1b[0m\n\n")

        (display "Color cube, 6x6x6:\n")
        (dotimes (green 6)
          (dotimes (red 6)
            (dotimes (blue 6)
              (let1 color (+ 16 (* 36 red) (* 6 green) blue)
                (display #"\x1b[48;5;~|color|m ")))
            (display "\x1b[0m "))
          (display "\n"))

        (display "Grayscale ramp:\n")
        (do-ec (: color 232 256)
               (display #"\x1b[48;5;~|color|m "))
        (display "\x1b[0m\n")))))

(define (main args)
  (grv-start-player))
