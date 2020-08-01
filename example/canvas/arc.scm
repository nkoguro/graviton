(use data.queue)
(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use math.const)
(use util.list)
(use util.match)

(define-class <mouse-event> (<jsevent>)
  ((width :js-property "target.width")
   (height :js-property "target.height")
   (client-width :js-property "target.clientWidth")
   (client-height :js-property "target.clientHeight")
   (offset-x :js-property "offsetX")
   (offset-y :js-property "offsetY")
   (x :allocation :virtual
      :slot-ref (lambda (obj)
                  (floor->exact (/. (* (slot-ref obj 'width) (slot-ref obj 'offset-x))
                                    (slot-ref obj 'client-width)))))
   (y :allocation :virtual
      :slot-ref (lambda (obj)
                  (floor->exact (/. (* (slot-ref obj 'height) (slot-ref obj 'offset-y))
                                    (slot-ref obj 'client-height)))))))


(define (main args)
  (grv-player)

  (grv-begin
    (match-let1 (w h) (force (client-window-size))
      (log-format "window width=~a, height=~a" w h))

    (capture-jsevent (client-window) "keyup" '("key"))

    (let1 canvas (make-canvas 150 200)
      (capture-jsevent canvas "click" <mouse-event>)

      (dotimes (i 4)
        (dotimes (j 3)
          (let ((x (+ 25 (* j 50)))
                (y (+ 25 (* i 50)))
                (radius 20)
                (start-angle 0)
                (end-angle (+ pi (/ (* pi j) 2)))
                (anti-clockwise (odd? (modulo i 2))))
            (begin-path)
            (arc x y radius start-angle end-angle anti-clockwise)
            (if (< 1 i)
              (fill)
              (stroke)))))

      (port-for-each (match-lambda
                       (('keyup _ "Escape")
                        (event-stream-close))
                       (('click _ mouse-event)
                        (log-format "mouse: x=~a, y=~a" (slot-ref mouse-event 'x) (slot-ref mouse-event 'y)))
                       (_
                        #f))
                     next-event)

      0)))
