(use gauche.uvector)
(use graviton)
(use graviton.grut)

(define-grut-window
  (canvas :context-2d ctx :width 300 :height 150))

(define (main args)
  (grv-player)

  (grv-begin
    (on-jsevent window "keyup" (key)
      (when (equal? key "Escape")
        (grv-exit)))

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
      (ctx'put-image-data image 20 20))))
