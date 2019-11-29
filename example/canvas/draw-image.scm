(use gauche.uvector)
(use graviton2)
(use math.const)

(define (main args)
  ;; (set-graviton-port! 8080)
  ;; (set-graviton-use-player! #f)
  (set-graviton-background-color! "#000")
  (grv-begin
    (let ((canvas (make-canvas 300 300))
          (loaded-image (load-canvas "example/font_16x16.png" :visible? #f)))
      (draw-canvas (await loaded-image) 0 0)))
  0)
