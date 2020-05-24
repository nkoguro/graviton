(use graviton)
(use graviton.canvas)
(use graviton.event)
(use math.const)

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (let1 canvas (make-canvas 300 150)
      (begin-path)
      (arc 100 75 50 0 (* 2 pi))
      (clip)
      (set-fill-style! "blue")
      (fill-rect 0 0 (slot-ref canvas 'width) (slot-ref canvas 'height))
      (set-fill-style! "orange")
      (fill-rect 0 0 100 100)))
  0)
