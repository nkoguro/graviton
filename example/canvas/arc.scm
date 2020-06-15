(use gauche.logger)
(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)
(use util.list)

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
    (receive (w h) (await (client-window-size))
      (log-format "window width=~a, height=~a" w h))
    (add-event-listener! (client-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (client-close))))

    (let1 canvas (make-canvas 150 200)
      (add-event-listener! canvas "click" <mouse-event>
        (lambda (event)
          #?=(slot-ref event 'x)
          #?=(slot-ref event 'y)))

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
                (stroke))))))))
