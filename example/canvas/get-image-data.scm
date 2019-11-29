(use gauche.logger)
(use gauche.uvector)
(use graviton2)
(use math.const)

(define (main args)
  ;; (set-graviton-port! 8080)
  ;; (set-graviton-use-player! #f)
  (grv-begin
    (let1 canvas (make-canvas 300 150)
      (rect 10 10 100 100)
      (fill)

      (let1 image (get-image-data 60 60 200 100)
        (log-format "image-data content length: ~a" (u8vector-length (await (download-image-data image))))
        (put-image-data image 150 10))))
  0)
