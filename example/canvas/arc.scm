(use data.queue)
(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)
(use text.html-lite)
(use util.list)
(use util.match)

(define (main args)
  (grv-player)

  (define-document-content
    (html:body
     (html:canvas :width 150 :height 200 :class "grv-object-fit-contain")))

  (grv-begin
    (receive (w h) (window-size)
      (log-format "window width=~a, height=~a" w h))

    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

    (let* ((canvas (document'query-selector "canvas"))
           (ctx (canvas'get-context "2d")))

      (on-jsevent canvas "click" (offset-x offset-y)
        (log-format "mouse: x=~a, y=~a" offset-x offset-y))

      (dotimes (i 4)
        (dotimes (j 3)
          (let ((x (+ 25 (* j 50)))
                (y (+ 25 (* i 50)))
                (radius 20)
                (start-angle 0)
                (end-angle (+ pi (/ (* pi j) 2)))
                (anti-clockwise (odd? (modulo i 2))))
            (ctx'begin-path)
            (ctx'arc x y radius start-angle end-angle anti-clockwise)
            (if (< 1 i)
              (ctx'fill)
              (ctx'stroke))))))))
