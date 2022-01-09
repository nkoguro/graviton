(use gauche.uvector)
(use graviton)
(use graviton.grut)

(define (main args)
  (with-window (grut-canvas-window 300 150)
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (let* ((image (ctx'create-image-data 100 100))
             (data (make-u8vector (* 100 100 4))))
        (do ((i 0 (+ i 4)))
            ((<= (u8vector-length data) i) #f)
          (u8vector-set! data (+ i 0) 190) ; R
          (u8vector-set! data (+ i 1) 0)   ; G
          (u8vector-set! data (+ i 2) 210) ; B
          (u8vector-set! data (+ i 3) 255) ; A
          )
        (image-data-update! image data)
        (ctx'put-image-data image 20 20)))))
