(use graviton)
(use graviton.grut)

(define (main args)
  (with-window #f
      ()
    (play-mml :main '(:adsr (0.05 0 1 0.05)
                      :wave-form (#f32(0.916993 1.37156 0.587542 0.224356 0.166988 -1.3045 -0.338598 -0.316463 -0.145825)
                                      #f32(0 0.921636 1.23641 0.812448 1.43946 0.7639 -0.310799 0.209386 -0.161906))
                      l8
                      c d e f g a b > c < b a g f e d c))
    (wait-all-tracks)
    (close-window)))
