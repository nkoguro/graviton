(use gauche.parameter)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)

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
        (display "Musette D-Dur BWV Anh.126\n")
        (when (is-browser?)
          (display "\nHit space key to play.\n")
          (while (not (equal? (jsevent-await window "keyup" '(key)) " "))
            #t))

        (play-mml :right '(:adsr (0.01 0.1 0.1 0.1)
                           :wave-form (#f32(0.77 0.06 0.08 0.03 0.03 0.01 0.01) #f32(0 0 0 0 0 0 0))
                           :tempo 90
                           l4
                           o5 a4 g16 f+16 e16 d16
                           o5 a4 g16 f+16 e16 d16
                           o4 f+16 g16 a8 g8 f+8
                           o4 e8 a8 f+8 d8

                           o5 a4 g16 f+16 e16 d16
                           o5 a4 g16 f+16 e16 d16
                           o4 f+16 g16 a8 g8 f+8
                           o4 e8 a8 (scope :adsr (0.01 0.1 0.5 0.1) d8. r16)

                           o5 c+16 d16 e8 c+16 d16 e8
                           o5 a8 e8 e4
                           o5 a8 e8 a8 e8
                           o5 d16 c+16 < b16 a16 b8 e8

                           o5 e8 d+8 < e8 > (scope :adsr (0.01 0.2 0.1 0.05) d4) c+8 a8 g+8
                           o5 e8 d+8 < e8 > (scope :adsr (0.01 0.2 0.1 0.05) d4) c+8 a8 g+8

                           o5 e16 d+16 c+16 d+16 e16 d+16 c+16 d+16
                           o5 e8 < g+8 a8 > d8
                           o5 c+16 d16 e8 < a8 d8
                           o4 c+16 d16 e8 < (scope :adsr (0.01 0.1 1 0.1) a4)
                           )
                  :left '(:adsr (0.01 0.1 0.1 0.1)
                          :wave-form (#f32(0.77 0.06 0.08 0.03 0.03 0.01 0.01) #f32(0 0 0 0 0 0 0))
                          :tempo 90

                          l4
                          o2 d8 > d8 < d8 > d8
                          o2 d8 > d8 < d8 > d8
                          o3 f+16 g16 a8 g8 f+8
                          o3 e8 a8 f+8 d8

                          o2 d8 > d8 < d8 > d8
                          o2 d8 > d8 < d8 > d8
                          o3 f+16 g16 a8 g8 f+8
                          o3 e8 a8 (scope :adsr (0.01 0.1 0.5 0.1) d8. r16)

                          o2 a8 > a8 < a8 > a8
                          o2 a8 > a8 < a8 > a8
                          o2 a8 > a8 < a8 > a8
                          o2 a8 > a8 < e8 > e8

                          o2 e8 > e8 < e8 > e8 < e8 > e8 < e8 > e8
                          o2 e8 > e8 < e8 > e8 < e8 > e8 < e8 > e8

                          o2 e8 > e8 < e8 > e8
                          o2 e8 > d8 c+8 d8
                          o3 (scope :adsr (0.01 0.2 0.1 0.05) e4) < a8 > d8
                          o3 c+16 d16 e8 < (scope :adsr (0.01 0.1 1 0.1) a4)))
        (wait-all-tracks)
        (display "Done\n")
        (close-window)))))
