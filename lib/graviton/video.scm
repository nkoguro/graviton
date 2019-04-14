;;;
;;; video.scm - Video
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

(define-module graviton.video
  (use gauche.hook)
  (use gauche.record)
  (use gauche.regexp)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.color)
  (use graviton.png)
  (use math.const)
  (use srfi-14)
  (use srfi-42)
  (use util.match)

  (export-all))

(select-module graviton.video)
(dynamic-load "graviton-video")

;;;
;;; Image
;;;

(define (save-image image filename :key (format 'png))
  (unless (eq? format 'png)
    (errorf "Unsupported format: ~s" format))
  (call-with-output-file filename
    (lambda (out)
      (let ((width (image-width image))
            (height (image-height image))
            (image-buffer (uvector-alias <u8vector> (image-rgba-pixels image))))
        (write-png-image width height image-buffer out)))))

(define (image-width image)
  (list-ref (image-size image) 0))

(define (image-height image)
  (list-ref (image-size image) 1))

(define (image? obj)
  (is-a? obj <graviton-image>))

(define (pixel-width image)
  (values-ref (pixel-size image) 0))

(define (pixel-height image)
  (values-ref (pixel-size image) 1))

(define (set-border! image v0 :optional (v1 #f) (v2 #f) (v3 #f))
  (let ((top #f)
        (bottom #f)
        (left #f)
        (right #f))
    (cond
      ((not v1)                         ; # of args = 1
       (set! top v0)
       (set! right v0)
       (set! bottom (- v0))
       (set! left (- v0)))
      ((not v2)                         ; # of args = 2
       (set! top v0)
       (set! right v1)
       (set! bottom (- v0))
       (set! left (- v1)))
      ((not v3)                         ; # of args = 3
       (set! top v0)
       (set! right v1)
       (set! bottom v2)
       (set! left (- v1)))
      (else                             ; # of args = 4
       (set! top v0)
       (set! right v1)
       (set! bottom v2)
       (set! left v3)))
    (set-image-border! image top right bottom left)))

(define (border-min-x image)
  (min (border-left image) (border-right image)))

(define (border-max-x image)
  (max (border-left image) (border-right image)))

(define (border-min-y image)
  (min (border-top image) (border-bottom image)))

(define (border-max-y image)
  (max (border-top image) (border-bottom image)))

(define (divide-image image width height)
  (let* ((img-w (image-width image))
         (img-h (image-height image))
         (nx (ceiling->exact (/ img-w width)))
         (ny (ceiling->exact (/ img-h height)))
         (len (* nx ny))
         (vec (make-vector len)))
    (dotimes (y ny)
      (dotimes (x nx)
        (vector-set! vec
                     (+ (* y nx) x)
                     (make-tile-image image
                                      (* x width)
                                      (* y height)
                                      (min width (- img-w (* x width)))
                                      (min height (- img-h (* y height)))))))
    vec))


;;;
;;; Window
;;;

(define (window-physical-width window)
  (values-ref (window-physical-size window) 0))

(define (window-physical-height window)
  (values-ref (window-physical-size window) 1))

(define (window-logical-width window)
  (values-ref (window-logical-size window) 0))

(define (window-logical-height window)
  (values-ref (window-logical-size window) 1))

(define (set-window-maximized! window maximized?)
  (if maximized?
      (maximize-window window)
      (restore-window window)))

(define (set-window-minimized! window minimized?)
  (if minimized?
      (minimize-window window)
      (restore-window window)))

(define (set-window-handler! window event proc)
  (hash-table-set! (window-handler-table window) event proc))

(define (last-window)
  (let1 wins (all-windows)
    (cond
      ((null? wins)
       #f)
      (else
       (car wins)))))

(define (window? obj)
  (is-a? obj <graviton-window>))

(define-method center-point ((window <graviton-window>))
  (window-center-point window))

(define-method center-point ((image <graviton-image>))
  (image-center-point image))

(define (center-x window-or-image)
  (list-ref (center-point window-or-image) 0))

(define (center-y window-or-image)
  (list-ref (center-point window-or-image) 1))


(set! (setter window-fullscreen?) set-window-fullscreen!)
(set! (setter window-position) set-window-position!)
(set! (setter window-title) set-window-title!)
(set! (setter window-resizable?) set-window-resizable!)
(set! (setter window-icon) set-window-icon!)
(set! (setter window-maximized?) set-window-maximized!)
(set! (setter window-minimized?) set-window-minimized!)

(define make-window-hook (make-hook 1))

;;;
;;; Sprite
;;;

(set! (setter sprite-image) set-sprite-image!)
(set! (setter sprite-x) set-sprite-x!)
(set! (setter sprite-y) set-sprite-y!)
(set! (setter sprite-z) set-sprite-z!)
(set! (setter sprite-angle) set-sprite-angle!)
(set! (setter sprite-zoom) set-sprite-zoom!)
(set! (setter sprite-visible?) set-sprite-visible!)
(set! (setter sprite-color) set-sprite-color!)


;;;
;;; TileMap
;;;

(set! (setter tile-map-offset) set-tile-map-offset!)


;;;
;;; Virtual Text Driver
;;;

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

;;;
;;; Draw
;;;

(define-record-type (<point> (pseudo-rtd <list>))
  make-point point?
  (x point-x)
  (y point-y))

(define (draw-point image x y color :key (thickness 0))
  (cond
    ((= thickness 0)
     (draw-rect image x y (pixel-width image) (pixel-height image) color))
    (else
     (draw-circle image x y (/. thickness 2) color :fill? #t))))

(define (draw-rect image x y w h color :key (fill? #f) (thickness 0))
  (cond
    ((= thickness 0)
     (%draw-rect image x y w h color fill?))
    (else
     (let ((x0 x)
           (y0 y)
           (x1 (+ x w))
           (y1 (+ x h)))
       (draw-polygon image
                     (list (make-point x0 y0)
                           (make-point x0 y1)
                           (make-point x1 y1)
                           (make-point x1 y0))
                     color
                     :fill? fill?
                     :thinkness thickness)))))

(define (draw-line image points color :key (thickness 0))
  (cond
    ((= thickness 0)
     (%draw-line image points color))
    ((null? points)
     #f)
    (else
     (draw-point image (point-x (car points)) (point-y (car points)) color :thickness thickness)
     (let loop ((point0 (car points))
                (points (cdr points)))
       (cond
         ((null? points)
          #f)
         ((< (abs (- (point-y point0) (point-y (car points)))) (pixel-height image))
          (let* ((x0 (point-x point0))
                 (y0 (point-y point0))
                 (x1 (point-x (car points)))
                 (y1 (point-y (car points)))
                 (x00 x0)
                 (x01 x0)
                 (x10 x1)
                 (x11 x1)
                 (d (/. thickness 2))
                 (y00 (- y0 d))
                 (y01 (+ y0 d))
                 (y10 (- y1 d))
                 (y11 (+ y1 d)))
            (draw-polygon image
                          (list (make-point x00 y00)
                                (make-point x01 y01)
                                (make-point x11 y11)
                                (make-point x10 y10))
                          color
                          :fill? #t)
            (draw-point image x1 y1 color :thickness thickness)
            (loop (car points) (cdr points))))
         ((< (abs (- (point-x point0) (point-x (car points)))) (pixel-width image))
          (let* ((x0 (point-x point0))
                 (y0 (point-y point0))
                 (x1 (point-x (car points)))
                 (y1 (point-y (car points)))
                 (d (/. thickness 2))
                 (x00 (- x0 d))
                 (x01 (+ x0 d))
                 (x10 (- x1 d))
                 (x11 (+ x1 d))
                 (y00 y0)
                 (y01 y0)
                 (y10 y1)
                 (y11 y1))
            (draw-polygon image
                          (list (make-point x00 y00)
                                (make-point x01 y01)
                                (make-point x11 y11)
                                (make-point x10 y10))
                          color
                          :fill? #t)
            (draw-point image x1 y1 color :thickness thickness)
            (loop (car points) (cdr points))))
         (else
          (let* ((x0 (point-x point0))
                 (y0 (point-y point0))
                 (x1 (point-x (car points)))
                 (y1 (point-y (car points)))
                 (a (/. (- y0 y1) (- x0 x1)))
                 (c (* thickness (sqrt (/. (* a a) (+ (* a a) 1))) 0.5))
                 (x00 (- x0 c))
                 (x01 (+ x0 c))
                 (x10 (- x1 c))
                 (x11 (+ x1 c))
                 (y00 (+ (* (/. -1 a) (- x00 x0)) y0))
                 (y01 (+ (* (/. -1 a) (- x01 x0)) y0))
                 (y10 (+ (* (/. -1 a) (- x10 x1)) y1))
                 (y11 (+ (* (/. -1 a) (- x11 x1)) y1)))
            (draw-polygon image
                          (list (make-point x00 y00)
                                (make-point x01 y01)
                                (make-point x11 y11)
                                (make-point x10 y10))
                          color
                          :fill? #t)
            (draw-point image x1 y1 color :thickness thickness)
            (loop (car points) (cdr points)))))))))

(define (draw-polygon image points color :key (fill? #f) (thickness 0))
  (cond
    ((= thickness 0)
     (%draw-polygon image points color fill?))
    ((null? points)
     #f)
    (else
     (draw-line image (cons (last points) points) color :thickness thickness)
     (when fill?
       (%draw-polygon image points color #t)))))

(define (draw-circle image
                     center-x
                     center-y
                     radius
                     color
                     :key
                     (start 0)
                     (angle #f)
                     (radius-ratio 1.0)
                     (rotate 0)
                     (fill? #f)
                     (draw-radius? (if angle #t #f))
                     (thickness 0))
  (define rotate-point
    (let ((m00 (cos rotate))
          (m01 (- (sin rotate)))
          (m10 (sin rotate))
          (m11 (cos rotate)))
      (lambda (x y)
        (let ((x1 (- x center-x))
              (y1 (- y center-y)))
          (make-point (+ (* m00 x1) (* m01 y1) center-x)
                      (+ (* m10 x1) (* m11 y1) center-y))))))

  (let ((dt (/ pi/2 (/ radius (receive (w h) (pixel-size image)
                                (min w h)))))
        (et (+ start (or angle (* 2 pi)))))
    (let loop ((t start)
               (points (if (or draw-radius?
                               (and angle fill?))
                           (list center-point)
                           '())))
      (cond
        ((<= et t)
         (let ((x (+ center-x (* radius (cos et))))
               (y (+ center-y (* (* radius radius-ratio) (sin et)))))
           (if (or draw-radius? fill?)
               (draw-polygon image (cons (rotate-point x y) points) color :fill? fill? :thickness thickness)
               (draw-line image (cons (rotate-point x y) points) color :thickness thickness))))
        (else
         (let ((x (+ center-x (* radius (cos t))))
               (y (+ center-y (* (* radius radius-ratio) (sin t)))))
           (loop (+ t dt) (cons (rotate-point x y) points))))))))

