(use gauche.logger)
(use graviton)
(use graviton.grut)
(use math.const)
(use text.html-lite)

(grv-window
  :path "/"
  :body
  (html:body
   (html:canvas :id "canvas" :class "grut-contain" :width 150 :height 200))

  (let-elements (canvas)
    (let1 ctx (canvas'get-context "2d")
      (log-format "window width=~a, height=~a" (~ window'inner-width) (~ window'inner-height))

      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (grv-exit)))

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

(define (main args)
  (grv-start-player))
