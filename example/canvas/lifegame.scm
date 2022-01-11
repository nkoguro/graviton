(use graviton)
(use graviton.grut)
(use srfi-27)
(use util.match)

(define *width* 200)
(define *height* 200)
(define *init-density* 0.5)

(define (render-field ctx field)
  (set! (~ ctx'fill-style) "black")
  (ctx'fill-rect 0 0 *width* *height*)

  (set! (~ ctx'fill-style) "white")
  (hash-table-for-each field (lambda (x+y alive?)
                               (when alive?
                                 (match-let1 (x . y) x+y
                                   (ctx'fill-rect x y 1 1))))))

(define (compute-counter-table field)
  (define adjacents '((-1 -1) (0 -1) (1 -1)
                      (-1 0) (0 0) (1 0)
                      (-1 1) (0 1) (1 1)))
  (let1 counter-table (make-hash-table 'equal?)
    (hash-table-for-each field
                         (lambda (x+y v)
                           (when v
                             (match-let1 (x . y) x+y
                               (for-each (match-lambda
                                           ((dx dy)
                                            (hash-table-update!
                                              counter-table
                                              (cons (modulo (+ x dx) *width*) (modulo (+ y dy) *height*))
                                              (cut + 1 <>)
                                              0)))
                                         adjacents)))))
    counter-table))

(define (compute-new-field field)
  (let ((new-field (make-hash-table 'equal?))
        (counter-table (compute-counter-table field)))
    (hash-table-for-each
      counter-table
      (lambda (x+y cnt)
        (let1 alive? (hash-table-get field x+y #f)
        (when (or (= cnt 3)
                  (and (= cnt 4) alive?))
          (hash-table-put! new-field x+y #t)))))
    new-field))

(define (main args)
  (random-source-randomize! default-random-source)

  (with-window (grut-canvas-window *width* *height* :background-color "black")
      (canvas)
    (let1 ctx (canvas'get-context "2d")
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (let1 field (make-hash-table 'equal?)
        (dotimes (_ (round->exact (* (* *width* *height*) *init-density*)))
          (hash-table-put! field (cons (random-integer *width*) (random-integer *height*)) #t))

        (on-animation-frame :priority 'low (t)
          (render-field ctx field)
          (set! field (compute-new-field field)))))))
