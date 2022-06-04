(use gauche.logger)
(use gauche.parseopt)
(use graviton)
(use text.html-lite)

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
    (with-window (grv-window
                   :body (html:body
                          (html:canvas :id "canvas" :width 300 :height 150))
                   :show? #f)
        (canvas)
      (let ((ctx (canvas'get-context "2d"))
            (text "Hello world"))
        (log-format "The text width of '~a' is ~a px" text (~ (ctx'measure-text text) 'width))
        (close-window)))))
