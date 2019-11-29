(use gauche.uvector)
(use graviton2)
(use math.const)

(define (main args)
  ;; (set-graviton-port! 8080)
  (grv-begin
    (let1 canvas (make-canvas 300 150)
      (let ((image (create-image-data 100 100))
            (data (make-u8vector (* 100 100 4))))
        (do ((i 0 (+ i 4)))
            ((<= (u8vector-length data) i) #f)
          (u8vector-set! data (+ i 0) 190) ; R
          (u8vector-set! data (+ i 1) 0)   ; G
          (u8vector-set! data (+ i 2) 210) ; B
          (u8vector-set! data (+ i 3) 255) ; A
          )
        (upload-image-data image data)
        (put-image-data image 20 20))))
  0)
