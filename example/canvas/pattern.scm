(use file.util)
(use graviton)
(use graviton.canvas)
(use graviton.event)

(define *program-dir* (sys-dirname (current-load-path)))

(define (main args)
  (grv-player)

  (grv-begin
    (add-event-listener! (browser-window) "keyup"
                         '("key")
      (lambda (key)
        (when (equal? key "Escape")
          (app-close))))

    (let1 pat (load-canvas (build-path *program-dir* "Canvas_createpattern.png") :visible? #f)
      (make-canvas 300 300)
      (set-fill-style! (pattern (await pat)))
      (fill-rect 0 0 300 300))))
