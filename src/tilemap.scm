;;;
;;; tilemap.scm - Tilemap
;;;
;;;   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
;;;   All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(inline-stub
  (define-cfn tile-map-offset-index (gtilemap::GrvTileMap* x::int y::int)
    ::int
    (unless (and (<= 0 x) (< x (-> gtilemap columns)))
      (Scm_Error "x is out of range: %d" x))
    (unless (and (<= 0 y) (< y (-> gtilemap rows)))
      (Scm_Error "y is out of range: %d" y))
    (return (% (+ (-> gtilemap offset) (* y (-> gtilemap columns)) x)
               (* (-> gtilemap columns) (-> gtilemap rows)))))

  (define-cfn tile-map-pos-index (gtilemap::GrvTileMap* x::int y::int)
    ::int
    (unless (and (<= 0 x) (< x (-> gtilemap columns)))
      (Scm_Error "x is out of range: %d" x))
    (unless (and (<= 0 y) (< y (-> gtilemap rows)))
      (Scm_Error "y is out of range: %d" y))
    (return (% (+ (* y (-> gtilemap columns)) x)
               (* (-> gtilemap columns) (-> gtilemap rows)))))

  (define-cfn equal-attr? (attr1::GrvAttribute* attr2::GrvAttribute*)
    ::bool
    (cond
      ((and (== attr1 NULL)
            (== attr2 NULL))
       (return true))
      ((== attr1 NULL)
       (return (and (== (-> attr2 foreground-color) #xffffffff)
                    (== (-> attr2 background-color) 0))))
      ((== attr2 NULL)
       (return (and (== (-> attr1 foreground-color) #xffffffff)
                    (== (-> attr1 background-color) 0))))
      (else
       (return (and (== (-> attr1 foreground-color) (-> attr2 foreground-color))
                    (== (-> attr1 background-color) (-> attr2 background-color)))))))

  (define-cfn update-tile-map (gtilemap::GrvTileMap* x::int y::int)
    ::void
    (let* ((pos-index::int (tile-map-pos-index gtilemap x y))
           (offset-index::int (tile-map-offset-index gtilemap x y))
           (tile-index::Uint32 (aref (-> gtilemap tiles) offset-index))
           (attr::GrvAttribute* (aref (-> gtilemap attrs) offset-index))
           (buf-tile-index::Uint32 (aref (-> gtilemap buf-tiles) pos-index))
           (buf-attr::GrvAttribute* (aref (-> gtilemap buf-attrs) pos-index)))
      (unless (and (== tile-index buf-tile-index)
                   (equal-attr? attr buf-attr))
        (let* ((dstrect::SDL_Rect)
               (fg-color::Uint32 #xffffffff)
               (gimage::GrvImage* (GRV_IMAGE_PTR (-> gtilemap image))))
          (set! (aref (-> gtilemap buf-tiles) pos-index) tile-index
                (aref (-> gtilemap buf-attrs) pos-index) attr
                (ref dstrect x) (* (-> gtilemap tile-width) x)
                (ref dstrect y) (* (-> gtilemap tile-height) y)
                (ref dstrect w) (-> gtilemap tile-width)
                (ref dstrect h) (-> gtilemap tile-height))
          (cond
            (attr
             (when (< (SDL_FillRect (-> gimage surface) (& dstrect) (-> attr background-color)) 0)
               (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))
             (set! fg-color (-> attr foreground-color)))
            (else
             (when (< (SDL_FillRect (-> gimage surface) (& dstrect) 0) 0)
               (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))))
          (let* ((tile (Scm_VectorRef (SCM_VECTOR (-> gtilemap tile-images)) tile-index SCM_UNBOUND))
                 (gtile::GrvTileImage*))
            (unless (GRV_TILE_IMAGE_P tile)
              (Scm_Error "<graviton-tile-image> required, but got %S" tile))
            (set! gtile (GRV_TILE_IMAGE_PTR tile))
            (bitblt (GRV_IMAGE_PTR (-> gtile image))
                    (& (-> gtile rect))
                    gimage
                    (& dstrect)
                    fg-color))))))
  ) ;; end of inline-stub

(define-cproc make-tile-map (window tile-images::<vector> columns::<int> rows::<int> x::<double> y::<double> :key (z::<double> 0) (fill::<uint32> 0))
  ::<graviton-tile-map>
  (let* ((gtilemap::GrvTileMap* (SCM_NEW GrvTileMap))
         (size::int (* columns rows))
         (tile-width::int)
         (tile-height::int)
         (sprite))
    (when (== (SCM_VECTOR_SIZE tile-images) 0)
      (Scm_Error "tile-images must have at least one element"))
    (dotimes (i (SCM_VECTOR_SIZE tile-images))
      (unless (GRV_TILE_IMAGE_P (Scm_VectorRef tile-images i SCM_UNBOUND))
        (Scm_Error "<graviton-tile-image> required, but got %S" (Scm_VectorRef tile-images i SCM_UNBOUND))))
    (let* ((tile-image (Scm_VectorRef tile-images 0 SCM_UNBOUND)))
      (set! tile-width (ref (-> (GRV_TILE_IMAGE_PTR tile-image) rect) w)
            tile-height (ref (-> (GRV_TILE_IMAGE_PTR tile-image) rect) h)))

    (set! (-> gtilemap tiles) (SCM_NEW_ATOMIC_ARRAY (.type Uint32) size)
          (-> gtilemap attrs) (SCM_NEW_ARRAY (.type GrvAttribute*) size)
          (-> gtilemap buf-tiles) (SCM_NEW_ATOMIC_ARRAY (.type Uint32) size)
          (-> gtilemap buf-attrs) (SCM_NEW_ARRAY (.type GrvAttribute*) size))
    (dotimes (i size)
      (set! (aref (-> gtilemap tiles) i) fill
            (aref (-> gtilemap attrs) i) NULL
            (aref (-> gtilemap buf-tiles) i) (lognot fill) ;; enforce initial update
            (aref (-> gtilemap buf-attrs) i) NULL))

    (set! (-> gtilemap columns) columns
          (-> gtilemap rows) rows
          (-> gtilemap offset) 0

          (-> gtilemap image) (Scm_EvalRec (SCM_LIST3 'make-image
                                                      (SCM_MAKE_INT (* tile-width columns))
                                                      (SCM_MAKE_INT (* tile-height rows)))
                                           graviton-module)
          (-> gtilemap tile-images) (SCM_OBJ tile-images)
          (-> gtilemap tile-width) tile-width
          (-> gtilemap tile-height) tile-height

          sprite (Scm_EvalRec (Scm_List 'make-sprite
                                        window
                                        ':image (-> gtilemap image)
                                        ':x (Scm_MakeFlonum x)
                                        ':y (Scm_MakeFlonum y)
                                        ':z (Scm_MakeFlonum z)
                                        NULL)
                              graviton-module))
    (dotimes (y rows)
      (dotimes (x columns)
        (update-tile-map gtilemap x y)))

    (return gtilemap)))

(define-cproc tile-map-tile-index (gtilemap::<graviton-tile-map> x::<int> y::<int>)
  ::<uint32>
  (return (aref (-> gtilemap tiles) (tile-map-offset-index gtilemap x y))))

(define-cproc tile-map-foreground-color (gtilemap::<graviton-tile-map> x::<int> y::<int>)
  ::<uint32>
  (let* ((attr::GrvAttribute* (aref (-> gtilemap attrs) (tile-map-offset-index gtilemap x y))))
    (cond
      (attr
       (return (-> attr foreground-color)))
      (else
       (return #xffffffff)))))

(define-cproc tile-map-background-color (gtilemap::<graviton-tile-map> x::<int> y::<int>)
  ::<uint32>
  (let* ((attr::GrvAttribute* (aref (-> gtilemap attrs) (tile-map-offset-index gtilemap x y))))
    (cond
      (attr
       (return (-> attr foreground-color)))
      (else
       (return #xffffffff)))))

(define-cproc set-tile-map-tile! (gtilemap::<graviton-tile-map>
                                  x::<int>
                                  y::<int>
                                  tile-index::<uint32>
                                  :key
                                  (foreground-color::<uint32> #xffffffff)
                                  (background-color::<uint32> 0))
  ::<void>
  (let* ((offset-index::int (tile-map-offset-index gtilemap x y)))
    (set! (aref (-> gtilemap tiles) offset-index) tile-index)
    (cond
      ((and (== foreground-color #xffffffff) (== background-color 0))
       (set! (aref (-> gtilemap attrs) offset-index) NULL))
      (else
       (let* ((attr::GrvAttribute* (SCM_NEW GrvAttribute)))
         (set! (-> attr foreground-color) foreground-color
               (-> attr background-color) background-color
               (aref (-> gtilemap attrs) offset-index) attr)))))
  (update-tile-map gtilemap x y))

(define-cproc tile-map-offset (gtilemap::<graviton-tile-map>)
  ::<int>
  (return (-> gtilemap offset)))

(define-cproc set-tile-map-offset! (gtilemap::<graviton-tile-map> offset::<int>)
  ::<void>
  (let* ((size::int (* (-> gtilemap columns) (-> gtilemap rows))))
    (while (< offset 0)
      (set! offset (+ offset size)))
    (set! (-> gtilemap offset) offset))
  (dotimes (y (-> gtilemap rows))
    (dotimes (x (-> gtilemap columns))
      (update-tile-map gtilemap x y))))

(define-cproc tile-map-sprite (gtilemap::<graviton-tile-map>)
  (return (-> gtilemap sprite)))

(define-cproc tile-map-tile-images (gtilemap::<graviton-tile-map>)
  (return (-> gtilemap tile-images)))

(define-cproc tile-map-columns (gtilemap::<graviton-tile-map>)
  ::<int>
  (return (-> gtilemap columns)))

(define-cproc tile-map-rows (gtilemap::<graviton-tile-map>)
  ::<int>
  (return (-> gtilemap rows)))


;; Virtual Text Driver
(define (code4->color n)
  (case n
    ((30) (rgb 0 0 0))
    ((40) (rgba 0 0 0 0))
    ((31 41) (rgb 170 0 0))
    ((32 42) (rgb 0 170 0))
    ((33 43) (rgb 170 85 0))
    ((34 44) (rgb 0 0 170))
    ((35 45) (rgb 170 0 170))
    ((36 46) (rgb 0 170 170))
    ((37 47) (rgb 170 170 170))
    ((90 100) (rgb 85 85 85))
    ((91 101) (rgb 255 85 85))
    ((92 102) (rgb 85 255 85))
    ((93 103) (rgb 255 255 85))
    ((94 104) (rgb 85 85 255))
    ((95 105) (rgb 255 85 255))
    ((96 106) (rgb 85 255 255))
    ((97 107) (rgb 255 255 255))
    (else
     (rgb 255 255 255))))

(define (code8->color n fg-color?)
  (cond
    ((<= 0 n 7)
     (code4->color (+ (if fg-color? 30 40) n)))
    ((<= 8 n 15)
     (code4->color (+ (if fg-color? 90 100) (- n 8))))
    ((<= 16 n 231)
     (let* ((v (- n 16))
            (d (/ 255 5))
            (b (* d (modulo v 6)))
            (g (* d (modulo (quotient v 6) 6)))
            (r (* d (quotient v 36))))
       (rgb r g b)))
    ((<= 232 n 255)
     (let1 v (round->exact (* (/ 255 23) (- n 232)))
       (rgb v v v)))
    (else
     (rgb 255 255 255))))

(define (make-text-driver tile-map)
  (let ((cursor-x 0)
        (cursor-y 0)
        (fg-color #xffffffff)
        (bg-color 0)
        (state 'normal)
        (escape-sequence '())
        (saved-cursor-position '())
        (esc-charset (list->char-set '(#\[ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\;))))
    (define (forward-cursor)
      (inc! cursor-x)
      (when (<= (tile-map-columns tile-map) cursor-x)
        (set! cursor-x 0)
        (inc! cursor-y))
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (backword-cursor)
      (dec! cursor-x)
      (when (< cursor-x 0)
        (set! cursor-x (- (tile-map-columns tile-map) 1))
        (dec! cursor-y))
      (when (< cursor-y 0)
        (set! cursor-x 0)
        (set! cursor-y 0)))
    (define (up-cursor)
      (dec! cursor-y)
      (when (< cursor-y 0)
        (scroll-down)
        (set! cursor-y 0)))
    (define (down-cursor)
      (inc! cursor-y)
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (htab n)
      (set! cursor-x (* (+ (quotient cursor-x n) 1) n))
      (when (<= (tile-map-columns tile-map) cursor-x)
        (set! cursor-x 0)
        (inc! cursor-y))
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (lf)
      (set! cursor-x 0)
      (inc! cursor-y)
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (cr)
      (set! cursor-x 0))
    (define (delete-char)
      (set-tile-map-tile! tile-map cursor-x cursor-y 0))
    (define (clear-line y)
      (dotimes (x (tile-map-columns tile-map))
        (set-tile-map-tile! tile-map x y 0)))
    (define (clear-screen)
      (dotimes (y (tile-map-rows tile-map))
        (clear-line y)))
    (define (scroll-up)
      (inc! (tile-map-offset tile-map) (tile-map-columns tile-map))
      (clear-line (- (tile-map-rows tile-map) 1)))
    (define (scroll-down)
      (dec! (tile-map-offset tile-map) (tile-map-columns tile-map))
      (clear-line 0))
    (define (erase-in-display n)
      (case n
        ((0) ;; clear from cursor to end of screen.
         (erase-in-line 0)
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y (+ cursor-y 1) (tile-map-rows tile-map))
                (set-tile-map-tile! x y 0)))
        ((1) ;; clear from cursor to beginning of the screen.
         (erase-in-line 1)
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y 0 cursor-y)
                (set-tile-map-tile! x y 0)))
        ((2 3) ;; clear entire screen
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y 0 (tile-map-rows tile-map))
                (set-tile-map-tile! x y 0)))))
    (define (erase-in-line n)
      (case n
        ((0) ;; clear from cursor to end of line.
         (do-ec (: x cursor-x (tile-map-columns tile-map))
                (set-tile-map-tile! x cursor-y 0)))
        ((1) ;; clear from cursor to beginning of the line.
         (do-ec (: x 0 (+ cursor-x 1))
                (set-tile-map-tile! x cursor-y 0)))
        ((2) ;; clear entire line.
         (do-ec (: x 0 (tile-map-columns tile-map))
                (set-tile-map-tile! x cursor-y 0)))))
    (define (escape-sequence->string)
      (list->string (reverse escape-sequence)))
    (define (driver c)
      (case state
        ((normal)
         (let1 code (char->ucs c)
           (cond
             ((= code #x08)
              (backword-cursor)
              (delete-char))
             ((= code #x09)
              (htab 8))
             ((= code #x0a)
              (lf))
             ((= code #x0d)
              (cr))
             ((= code #x1b)
              (set! state 'escape))
             ((and (<= 0 code) (< code (vector-length (tile-map-tile-images tile-map))))
              (set-tile-map-tile! tile-map cursor-x cursor-y code :foreground-color fg-color :background-color bg-color)
              (forward-cursor)))))
        ((escape)
         (cond
           ((eqv? c #\[)
            (set! escape-sequence '())
            (set! state 'escape-sequence))
           (else
            (set! state 'normal))))
        ((escape-sequence)
         (cond
           ((char-set-contains? esc-charset c)
            (push! escape-sequence c))
           ((eqv? c #\A)
            ;; Cursor up
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (up-cursor)))
            (set! state 'normal))
           ((eqv? c #\B)
            ;; Cursor down
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (down-cursor)))
            (set! state 'normal))
           ((eqv? c #\C)
            ;; Cursor forward
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (forward-cursor)))
            (set! state 'normal))
           ((eqv? c #\D)
            ;; Cursor back
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (backward-cursor)))
            (set! state 'normal))
           ((eqv? c #\E)
            ;; Cursor next line
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x 0)
              (dotimes (_ n)
                (down-cursor)))
            (set! state 'normal))
           ((eqv? c #\F)
            ;; Cursor previous line
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x 0)
              (dotimes (_ n)
                (up-cursor)))
            (set! state 'normal))
           ((eqv? c #\G)
            ;; Cursor horizontal absolute
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x (min (- n 1) (- (tile-map-columns tile-map) 1))))
            (set! state 'normal))
           ((or (eqv? c #\H) (eqv? c #\f))
            ;; Cursor move position
            (rxmatch-case (escape-sequence->string)
              (#/(\d*)\;(\d*)/ (_ n m)
               (let ((row (min (or (string->number n) 1) (tile-map-rows tile-map)))
                     (col (min (or (string->number m) 1) (tile-map-columns tile-map))))
                 (set! cursor-x (- col 1))
                 (set! cursor-y (- row 1)))))
            (set! state 'normal))
           ((eqv? c #\J)
            ;; Erase in display
            (erase-in-display (or (string->number (escape-sequence->string)) 1))
            (set! state 'normal))
           ((eqv? c #\K)
            ;; Erase in line
            (erase-in-line (or (string->number (escape-sequence->string)) 1))
            (set! state 'normal))
           ((eqv? c #\S)
            ;; Scroll up
            (dotimes (_ (or (string->number (escape-sequence->string)) 1))
              (scroll-up))
            (set! state 'normal))
           ((eqv? c #\T)
            ;; Scroll down
            (dotimes (_ (or (string->number (escape-sequence->string)) 1))
              (scroll-down))
            (set! state 'normal))
           ((eqv? c #\s)
            ;; Save cursor position
            (push! saved-cursor-position (cons cursor-x cursor-y))
            (set! state 'normal))
           ((eqv? c #\u)
            ;; Restore cursor position
            (unless (null? saved-cursor-position)
              (match (pop! saved-cursor-position)
                ((x . y)
                 (set! cursor-x x)
                 (set! cursor-y y))))
            (set! state 'normal))
           ((eqv? c #\m)
            (rxmatch-case (escape-sequence->string)
              (#/^0?$/ (_)
               (set! fg-color #xffffffff)
               (set! bg-color 0))
              (#/^1\;3(\d)$/ (_ v)
               (set! fg-color (+ 90 (string->number v))))
              (#/^1\;4(\d)$/ (_ v)
               (set! fg-color (+ 100 (string->number v))))
              (#/^(\d+)$/ (_ v)
               (let1 v (or (string->number v) 0)
                 (cond
                   ((or (<= 30 v 37) (<= 90 v 97))
                    (set! fg-color (code4->color v)))
                   ((or (<= 40 v 47) (<= 100 v 107))
                    (set! bg-color (code4->color v))))))
              (#/^(\d+)\;(\d+)$/ (_ v0 v1)
               (for-each (lambda (v)
                           (let1 v (or (string->number v) 0)
                             (cond
                               ((or (<= 30 v 37) (<= 90 v 97))
                                (set! fg-color (code4->color v)))
                               ((or (<= 40 v 47) (<= 100 v 107))
                                (set! bg-color (code4->color v))))))
                         (list v0 v1)))
              (#/^38\;5\;(\d+)$/ (_ n)
               (set! fg-color (code8->color (or (string->number n) 0) #t)))
              (#/^48\;5\;(\d+)$/ (_ n)
               (set! bg-color (code8->color (or (string->number n) 0) #f)))
              (#/^38\;2\;(\d+)\;(\d+)\;(\d+)$/ (_ r g b)
               (set! fg-color (rgb (or (string->number r) 0)
                                   (or (string->number g) 0)
                                   (or (string->number b) 0))))
              (#/^48\;2\;(\d+)\;(\d+)\;(\d+)$/ (_ r g b)
               (set! bg-color (rgb (or (string->number r) 0)
                                   (or (string->number g) 0)
                                   (or (string->number b) 0)))))
            (set! state 'normal))
               ) ;; end of cond
              )  ;; end of escape-sequence in case
            )    ;; end of case
           )     ;; end of define
    driver))

(define (tile-map->output-port tile-map)
  (let ((out (make <virtual-output-port>))
        (driver (make-text-driver tile-map)))
    (slot-set! out 'putc driver)
    (slot-set! out 'putb (lambda (b)
                           (driver (ucs->char b))))
    out))

