;;;
;;; png.scm - PNG reader/writer
;;;
;;;   Copyright (c) 2016 KOGURO, Naoki (naoki@koguro.net)
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

;; Returns true if the signature is PNG.
(define (read-png-signature in)
  (equal? (read-uvector <u8vector> 8 in)
          #u8(137 80 78 71 13 10 26 10)))

;; Returns (chunk-type chunk-data).
(define (read-png-chunk in)
  (let* ((len (read-u32 in 'big-endian))
         (type&data (read-uvector <u8vector> (+ 4 len) in))
         (chunk-crc (read-u32 in 'big-endian))
         (chunk-type (u8vector->string type&data 0 (min 4 (u8vector-length type&data)))))
    (if (and (= (u8vector-length type&data) (+ 4 len))
             (= (crc32 (u8vector->string type&data)) chunk-crc))
        (values chunk-type (uvector-alias <u8vector> type&data 4))
        (values chunk-type #f))))

;; Returns (width height bit-depth color-type compression-method filter-method interlace-method).
(define (read-png-ihdr-chunk chunk)
  (let1 in (open-input-uvector chunk)
    (let* ((width (read-u32 in 'big-endian))
           (height (read-u32 in 'big-endian))
           (bit-depth (read-u8 in))
           (color-type (read-u8 in))
           (compression-method (read-u8 in))
           (filter-method (read-u8 in))
           (interlace-method (read-u8 in)))
      (values width height bit-depth color-type compression-method filter-method interlace-method))))

;; Returns palette u32vector (the element is rgb).
;; If the chunk length can't be divided by 3, returns #f.
(define (read-png-plte-chunk chunk)
  (let1 len (u8vector-length chunk)
    (let-values (((r q) (div-and-mod len 3)))
      (if (= q 0)
          (let1 plte (make-u32vector r 0)
            (dotimes (i r)
              (u32vector-set! plte i (logior (ash (u8vector-ref chunk (* i 3)) 16)
                                             (ash (u8vector-ref chunk (+ (* i 3) 1)) 8)
                                             (u8vector-ref chunk (+ (* i 3) 2)))))
            plte)
          #f))))

;; Returns background color. (r b g)
(define (read-png-bkgd-chunk chunk bit-depth color-type plte)
  (let* ((in (open-input-uvector chunk))
         (readv (cond
                  ((= color-type PNG-COLOR-INDEX)
                   (cut read-u8 in))
                  (else
                   (if (= bit-depth 16)
                       (^() (ash (read-u16 in) -8))
                       (cut read-u8 in))))))
    (cond
      ((or (= color-type PNG-COLOR-GRAY)
           (= color-type PNG-COLOR-GRAY-ALPHA))
       (let1 v (readv)
         (values v v v)))
      ((or (= color-type PNG-COLOR-TRUE)
           (= color-type PNG-COLOR-TRUE-ALPHA))
       (let* ((r (readv))
              (g (readv))
              (b (readv)))
         (values r g b)))
      ((= color-type PNG-COLOR-INDEX)
       (let1 rgb (u32vector-ref plte (readv))
         (values (ash rgb -16) (logand (ash rgb -8) #xff) (logand rgb #xff)))))))

;; Returns trns samply value or vector.
;; For PNG-COLOR-GRAY, an integer (gray sample value) will be returned.
;; For PNG-COLOR-TRUE, a vector (red sample value, green sample value and blue sample value) will be returned.
;; For PNG-COLOR-INDEX, an u8vector which contains alpha value for each indexes will be returned.
(define (read-png-trns-chunk chunk color-type)
  (let1 in (open-input-uvector chunk)
    (cond
      ((= color-type PNG-COLOR-GRAY)
       (read-u16 in 'big-endian))
      ((= color-type PNG-COLOR-TRUE)
       (let* ((rv (read-u16 in 'big-endian))
              (gv (read-u16 in 'big-endian))
              (bv (read-u16 in 'big-endian)))
         (vector rv gv bv)))
      ((= color-type PNG-COLOR-INDEX)
       (let1 trns (make-u8vector 256 255)
         (generator-for-each (lambda (i v)
                               (u8vector-set! trns i v))
                             (giota)
                             (cut read-u8 in))
         trns)))))

;; Returns image data.
(define (read-png-idat-chunk chunk width height bit-depth color-type)
  (let* ((line-len (* (cond
                        ((= color-type PNG-COLOR-GRAY) 1)
                        ((= color-type PNG-COLOR-TRUE) 3)
                        ((= color-type PNG-COLOR-INDEX) 1)
                        ((= color-type PNG-COLOR-GRAY-ALPHA) 2)
                        ((= color-type PNG-COLOR-TRUE-ALPHA) 4))
                      (cond
                        ((= bit-depth 1) (+ (ash (- width 1) -3) 1))
                        ((= bit-depth 2) (+ (ash (- width 1) -2) 1))
                        ((= bit-depth 4) (+ (ash (- width 1) -1) 1))
                        ((= bit-depth 8) width)
                        ((= bit-depth 16) (* 2 width)))))
         (buf (make-u8vector (* line-len height) 0)))
    (let ((zin (open-inflating-port (open-input-uvector chunk) :buffer-size 32768))
          (prev-buffer (make-u8vector line-len 0)))
      (dotimes (y height)
        (let* ((filter-type (read-u8 zin))
               (in-buffer (read-uvector <u8vector> line-len zin))
               (cur-buffer (uvector-alias <u8vector> buf (* y line-len) (* (+ y 1) line-len))))
          (filter-decoder! cur-buffer color-type filter-type prev-buffer in-buffer)
          (set! prev-buffer cur-buffer))))
    buf))

(define (read-png-image :optional (in (current-input-port)))
  (unless (read-png-signature in)
    (error "Not PNG data"))
  (let loop ((width #f)
             (height #f)
             (image-buffer #f)
             (bit-depth #f)
             (color-type #f)
             (interlace-method #f)
             (plte #f)
             (trns #f)
             (chunks '()))
    (let-values (((chunk-type chunk) (read-png-chunk in)))
      (cond
        ((not chunk-type)
         (error "Invalid PNG format"))
        ((equal? chunk-type "IHDR")
         (let-values (((width height bit-depth color-type compression-method filter-method interlace-method)
                       (read-png-ihdr-chunk chunk)))
           (loop width
                 height
                 (make-u8vector (* width height 4))
                 bit-depth
                 color-type
                 interlace-method
                 plte
                 (cond
                   ((= color-type PNG-COLOR-GRAY)
                    -1)
                   ((= color-type PNG-COLOR-TRUE)
                    #f)
                   ((= color-type PNG-COLOR-INDEX)
                    (make-u8vector 256 255))
                   (else
                    #f))
                 chunks)))
        ((equal? chunk-type "PLTE")
         (let1 plte (read-png-plte-chunk chunk)
           (loop width height image-buffer bit-depth color-type interlace-method plte trns chunks)))
        ((equal? chunk-type "IDAT")
         (loop width height image-buffer bit-depth color-type interlace-method plte trns (cons chunk chunks)))
        ((equal? chunk-type "IEND")
         (unless image-buffer
           (error "IHDR chunk not found"))
         (let1 buf (read-png-idat-chunk (apply u8vector-append (reverse chunks))
                                        width height bit-depth color-type)
           (cond
             ((= color-type PNG-COLOR-GRAY)
              (convert-from-gray! image-buffer width height bit-depth buf (or trns -1)))
             ((= color-type PNG-COLOR-TRUE)
              (convert-from-true! image-buffer width height bit-depth buf (or trns #(-1 -1 -1))))
             ((= color-type PNG-COLOR-INDEX)
              (unless plte
                (error "PLTE chunk not found"))
              (convert-from-index! image-buffer width height bit-depth buf plte (or trns (make-u8vector 256 255))))
             ((= color-type PNG-COLOR-GRAY-ALPHA)
              (convert-from-gray-alpha! image-buffer width height bit-depth buf))
             ((= color-type PNG-COLOR-TRUE-ALPHA)
              (convert-from-true-alpha! image-buffer width height bit-depth buf))
             (else
              (errorf "Invalid color-type: ~a" color-type)))
           img))
        ((equal? chunk-type "tRNS")
         (loop width height image-buffer bit-depth color-type interlace-method plte (read-png-trns-chunk chunk color-type) chunks))
        ((equal? chunk-type "bKGD")
         (let-values (((r g b)
                       (read-png-bkgd-chunk chunk bit-depth color-type plte)))
           (u32vector-fill! (uvector-alias <u32vector> image-buffer) (if (eq? (native-endian) 'big-endian)
                                                                         (logior (ash r 24)
                                                                                 (ash g 16)
                                                                                 (ash b 8)
                                                                                 #xff)
                                                                         (logior (ash a 24)
                                                                                 (ash b 16)
                                                                                 (ash g 8)
                                                                                 r))))
         (loop width height image-buffer bit-depth color-type interlace-method plte trns chunks))
        (else
         ;; Ignore other chunk.
         (loop width height image-buffer bit-depth color-type interlace-method plte trns chunks))))))

(define (write-png-chunk header proc out)
  (let ((header-and-data (call-with-output-string (lambda (out)
                                                    (display header out)
                                                    (proc out)))))
    (write-u32 (- (string-size header-and-data) 4) out 'big-endian)
    (display header-and-data out)
    (write-u32 (crc32 header-and-data) out 'big-endian)))

;; IHDR chunk
(define (write-png-ihdr-chunk width height bit-depth color-type interlace-method out)
  (write-png-chunk "IHDR"
    (lambda (out)
      (write-u32 width out 'big-endian)
      (write-u32 height out 'big-endian)
      (write-u8 bit-depth out)
      (write-u8 color-type out)
      (write-u8 0 out)
      (write-u8 0 out)
      (write-u8 interlace-method out))
    out))

;; IDAT chunk
;; filter-type can be #f. If filter-type is #f, try to find best filter type automatically.
;; TODO: implement filter-type = #f case
(define (write-png-idat-chunk color-type filter-type line-length buffer out)
  (write-png-chunk "IDAT"
    (lambda (out)
      (display (call-with-output-string
                 (lambda (out)
                   (let ((zout (open-deflating-port out
                                                    :buffer-size 32768
                                                    :strategy Z_RLE))
                         (outbuf (make-u8vector line-length 0))
                         (prvbuf (make-u8vector line-length 0)))
                     (do ((start 0 (+ start line-length)))
                         ((<= (u8vector-length buffer) start))
                       (write-u8 filter-type zout)
                       (let1 curbuf (uvector-alias <u8vector> buffer start (+ start line-length))
                         (filter-encoder! outbuf color-type filter-type prvbuf curbuf)
                         (write-block outbuf zout)
                         (set! prvbuf curbuf)))
                     (close-output-port zout))))
               out))
    out))

;; IEND chunk
(define (write-png-iend-chunk out)
  (write-png-chunk "IEND" (^p #f) out))

;; Returns color-type, bit-depth, data buffer, line-length, palette and trns.
;; If the color-type doesn't have a palette and trns, #f will be returned as the palette and trns.
(define (convert-optimized-buffer width height image-buffer)
  (let-values (((color-type inv-plte trns) (analyze-color image-buffer width height)))
    (cond
      ((= color-type PNG-COLOR-INDEX)
       (let-values (((bit-depth newbuf line-length) (convert-index-color image-buffer width height inv-plte)))
         (let1 plte (make-u8vector (* (hash-table-num-entries inv-plte) 3) 0)
           (hash-table-for-each inv-plte (lambda (rgb i)
                                           (u8vector-set! plte (* i 3) (ash rgb -16))
                                           (u8vector-set! plte (+ (* i 3) 1) (logand (ash rgb -8) #xff))
                                           (u8vector-set! plte (+ (* i 3) 2) (logand rgb #xff))))
           (values color-type bit-depth newbuf line-length plte trns))))
      ((= color-type PNG-COLOR-TRUE)
       (let-values (((bit-depth newbuf line-length) (convert-rgb-color image-buffer width height)))
         (values color-type 8 newbuf line-length #f #f)))
      ((= color-type PNG-COLOR-TRUE-ALPHA)
       (values color-type 8 image-buffer (* width 4) #f #f))
      (else
       (errorf "Invalid color-type: ~a" color-type)))))

(define (write-png-plte-chunk plte out)
  (write-png-chunk "PLTE" (cut write-uvector plte <>) out))

(define (write-png-trns-chunk trns out)
  (write-png-chunk "tRNS" (cut write-uvector trns <>) out))

(define (write-png-image width height image-buffer :optional (out (current-output-port)) (filter-type #f))
  (let-values (((color-type bit-depth buffer line-length plte trns) (convert-optimized-buffer width height image-buffer)))
    ;; PNG file signature
    (display #*"\x89PNG\x0d\x0a\x1a\x0a" out)
    ;; IHDR
    (write-png-ihdr-chunk width height bit-depth color-type PNG-INTERLACE-NONE out)
    ;; PLTE
    (when plte
      (write-png-plte-chunk plte out))
    ;; tRNS
    (when trns
      (write-png-trns-chunk trns out))
    ;; IDAT
    (write-png-idat-chunk color-type
                          (or filter-type
                              (cond
                                ((< bit-depth 8)
                                 PNG-FILTER-NONE)
                                (else
                                 PNG-FILTER-UP)))
                          line-length
                          buffer
                          out)
    ;; IEND
    (write-png-iend-chunk out)))

(inline-stub
 (declcode
  (.include <assert.h> <stdbool.h> <stdlib.h> <string.h>
            <gauche.h> <gauche/hash.h> <gauche/vector.h>))

 ;; PNG color type constants
 "#define PNG_COLOR_GRAY 0"
 "#define PNG_COLOR_TRUE 2"
 "#define PNG_COLOR_INDEX 3"
 "#define PNG_COLOR_GRAY_ALPHA 4"
 "#define PNG_COLOR_TRUE_ALPHA 6"
 (define-constant PNG-COLOR-GRAY 0)
 (define-constant PNG-COLOR-TRUE 2)
 (define-constant PNG-COLOR-INDEX 3)
 (define-constant PNG-COLOR-GRAY-ALPHA 4)
 (define-constant PNG-COLOR-TRUE-ALPHA 6)

 ;; PNG interlace methods
 "#define PNG_INTERLACE_NONE 0"
 "#define PNG_INTERLACE_ADAM7 1"
 (define-constant PNG-INTERLACE-NONE 0)
 (define-constant PNG-INTERLACE-ADAM7 1)

 ;; PNG filter type
 "#define PNG_FILTER_NONE 0"
 "#define PNG_FILTER_SUB 1"
 "#define PNG_FILTER_UP 2"
 "#define PNG_FILTER_AVERAGE 3"
 "#define PNG_FILTER_PAETH 4"
 (define-constant PNG-FILTER-NONE 0)
 (define-constant PNG-FILTER-SUB 1)
 (define-constant PNG-FILTER-UP 2)
 (define-constant PNG-FILTER-AVERAGE 3)
 (define-constant PNG-FILTER-PAETH 4)

 (define-cfn paeth-predictor (a::int b::int c::int) ::int :static
   (let* ((p::int (- (+ a b) c))
          (pa::int (abs (- p a)))
          (pb::int (abs (- p b)))
          (pc::int (abs (- p c))))
     (cond
       ((and (<= pa pb) (<= pa pc))
        (return a))
       ((<= pb pc)
        (return b))
       (else
        (return c)))))

 ;; bit-depth must be <8.
 (define-cproc filter-encoder! (out-buffer::<u8vector>
                                color-type::<int>
                                filter-type::<int>
                                prev-buffer::<u8vector>
                                cur-buffer::<u8vector>)
   ::<void>
   (SCM_ASSERT (== (SCM_U8VECTOR_SIZE out-buffer) (SCM_U8VECTOR_SIZE cur-buffer)))
   (SCM_ASSERT (== (SCM_U8VECTOR_SIZE out-buffer) (SCM_U8VECTOR_SIZE prev-buffer)))
   (let* ((d::int)
          (i::int))
     (cond
       ((== color-type PNG_COLOR_GRAY)
        (set! d 1))
       ((== color-type PNG_COLOR_GRAY_ALPHA)
        (set! d 2))
       ((== color-type PNG_COLOR_INDEX)
        (set! d 1))
       ((== color-type PNG_COLOR_TRUE)
        (set! d 3))
       ((== color-type PNG_COLOR_TRUE_ALPHA)
        (set! d 4)))
     (cond
       ((== filter-type PNG_FILTER_NONE)
        (memcpy (SCM_U8VECTOR_ELEMENTS out-buffer) (SCM_U8VECTOR_ELEMENTS cur-buffer) (SCM_U8VECTOR_SIZE out-buffer)))
       ((== filter-type PNG_FILTER_SUB)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT out-buffer i)
                (cast |unsigned char| (- (SCM_U8VECTOR_ELEMENT cur-buffer i)
                                         (?: (<= 0 (- i d))
                                             (SCM_U8VECTOR_ELEMENT cur-buffer (- i d))
                                             0))))))
       ((== filter-type PNG_FILTER_UP)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT out-buffer i)
                (cast |unsigned char| (- (SCM_U8VECTOR_ELEMENT cur-buffer i)
                                         (SCM_U8VECTOR_ELEMENT prev-buffer i))))))
       ((== filter-type PNG_FILTER_AVERAGE)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT out-buffer i)
                (cast |unsigned char| (- (SCM_U8VECTOR_ELEMENT cur-buffer i)
                                         (/ (+ (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT cur-buffer (- i d)) 0)
                                               (SCM_U8VECTOR_ELEMENT prev-buffer i))
                                            2))))))
       ((== filter-type PNG_FILTER_PAETH)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT out-buffer i)
                (cast |unsigned char| (- (SCM_U8VECTOR_ELEMENT cur-buffer i)
                                         (paeth-predictor (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT cur-buffer (- i d)) 0)
                                                          (SCM_U8VECTOR_ELEMENT prev-buffer i)
                                                          (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT prev-buffer (- i d)) 0))))))))))

 (define-cproc filter-decoder! (cur-buffer::<u8vector>
                                color-type::<int>
                                filter-type::<int>
                                prev-buffer::<u8vector>
                                in-buffer::<u8vector>)
   ::<void>
   (SCM_ASSERT (== (SCM_U8VECTOR_SIZE cur-buffer) (SCM_U8VECTOR_SIZE prev-buffer)))
   (SCM_ASSERT (== (SCM_U8VECTOR_SIZE cur-buffer) (SCM_U8VECTOR_SIZE in-buffer)))
   (let* ((d::int)
          (i::int))
     (cond
       ((== color-type PNG_COLOR_GRAY)
        (set! d 1))
       ((== color-type PNG_COLOR_GRAY_ALPHA)
        (set! d 2))
       ((== color-type PNG_COLOR_INDEX)
        (set! d 1))
       ((== color-type PNG_COLOR_TRUE)
        (set! d 3))
       ((== color-type PNG_COLOR_TRUE_ALPHA)
        (set! d 4)))
     (cond
       ((== filter-type PNG_FILTER_NONE)
        (memcpy (SCM_U8VECTOR_ELEMENTS cur-buffer) (SCM_U8VECTOR_ELEMENTS in-buffer) (SCM_U8VECTOR_SIZE cur-buffer)))
       ((== filter-type PNG_FILTER_SUB)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT cur-buffer i)
                (cast |unsigned char| (+ (SCM_U8VECTOR_ELEMENT in-buffer i)
                                         (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT cur-buffer (- i d)) 0))))))
       ((== filter-type PNG_FILTER_UP)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT cur-buffer i)
                (cast |unsigned char| (+ (SCM_U8VECTOR_ELEMENT in-buffer i)
                                         (SCM_U8VECTOR_ELEMENT prev-buffer i))))))
       ((== filter-type PNG_FILTER_AVERAGE)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT cur-buffer i)
                (cast |unsigned char| (+ (SCM_U8VECTOR_ELEMENT in-buffer i)
                                         (/ (+ (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT cur-buffer (- i d)) 0)
                                               (SCM_U8VECTOR_ELEMENT prev-buffer i))
                                            2))))))
       ((== filter-type PNG_FILTER_PAETH)
        (for ((set! i 0) (< i (SCM_U8VECTOR_SIZE cur-buffer)) (pre++ i))
          (set! (SCM_U8VECTOR_ELEMENT cur-buffer i)
                (cast |unsigned char| (+ (SCM_U8VECTOR_ELEMENT in-buffer i)
                                         (paeth-predictor (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT cur-buffer (- i d)) 0)
                                                          (SCM_U8VECTOR_ELEMENT prev-buffer i)
                                                          (?: (<= 0 (- i d)) (SCM_U8VECTOR_ELEMENT prev-buffer (- i d)) 0))))))))))


 (define-cfn draw-pixel (x::int image-buffer::ScmU8Vector* offset::int r::int g::int b::int a::int) ::void :static :inline
   (set! (aref (SCM_U8VECTOR_ELEMENTS image-buffer) (+ offset (* x 4) 0)) r)
   (set! (aref (SCM_U8VECTOR_ELEMENTS image-buffer) (+ offset (* x 4) 1)) g)
   (set! (aref (SCM_U8VECTOR_ELEMENTS image-buffer) (+ offset (* x 4) 2)) b)
   (set! (aref (SCM_U8VECTOR_ELEMENTS image-buffer) (+ offset (* x 4) 3)) a))

 (define-cise-stmt draw-pixels-and-forward!
   ((_ x image-buffer offset width bit-depth val ('lambda (v r g b a) body ...))
    (let ((i (gensym "loop"))
          (tmp (gensym "tmp"))
          (m (gensym "mask")))
      `(let* ((,v :: int 0)
              (,r :: int 0)
              (,g :: int 0)
              (,b :: int 0)
              (,a :: int 0))
         (case ,bit-depth
           ((1 2 4)
            (let* ((,m :: int 0)
                   (,tmp :: int ,val)
                   (,i :: int 0))
              (case ,bit-depth
                ((1) (set! ,m #x80))
                ((2) (set! ,m #xc0))
                ((4) (set! ,m #xf0)))
              (for ((set! ,i 0) (< ,i 8) (set! ,i (+ ,i ,bit-depth)))
                (set! ,v (>> (logand ,m ,tmp) (- 8 ,bit-depth)))
                (begin ,@body)
                (draw-pixel ,x ,image-buffer ,offset ,r ,g ,b ,a)
                (inc! ,x)
                (unless (< ,x ,width)
                  (break))
                (set! ,tmp (<< ,tmp ,bit-depth)))))
           (else
            (set! ,v ,val)
            (begin ,@body)
            (draw-pixel ,x ,image-buffer ,offset ,r ,g ,b ,a)
            (inc! ,x))))))
   ((_ x image-buffer offset width bit-depth ('lambda args body ...))
    (let* ((vals (drop-right args 4))
           (rgba (take-right args 4))
           (r (first rgba))
           (g (second rgba))
           (b (third rgba))
           (a (fourth rgba)))
      `(let* ((,r :: int 0)
              (,g :: int 0)
              (,b :: int 0)
              (,a :: int 0))
         (begin ,@body)
         (draw-pixel ,x ,image-buffer ,offset ,r ,g ,b ,a)
         (inc! ,x)))))

 (define-cise-stmt draw-image-buffer-from-idat
   ((_ image-buffer width height bit-depth buffer ('lambda (v r g b a) body ...))
    (let ((x (gensym "x"))
          (y (gensym "y"))
          (i (gensym "i"))
          (val (gensym "val"))
          (offset (gensym "offset"))
          (idat-data (gensym "idat"))
          (idat-length (gensym "idatlen")))
      `(begin
         (unless (SCM_UVECTORP buffer)
           (Scm_Error "buffer must be <u8vector>, but got %S" buffer))
         (let* ((,offset :: int 0)
                (,x :: int 0)
                (,y :: int 0)
                (,i :: int 0)
                (,v :: int 0)
                (,val :: int 0)
                (,idat-data :: |unsigned char*| (SCM_U8VECTOR_ELEMENTS ,buffer))
                (,idat-length :: int (SCM_U8VECTOR_SIZE ,buffer)))
           (dotimes (,y ,height)
             (set! ,x 0)
             (loop
              (unless (< ,i ,idat-length)
                (Scm_Error "idat data is too little to fill %Ax%A image" ,width ,height))
              (case ,bit-depth
                ((1 2 4 8)
                 (set! ,val (aref ,idat-data ,i))
                 (inc! ,i))
                ((16)
                 (set! ,val (logior (<< (aref ,idat-data ,i) 8)
                                    (aref ,idat-data (+ ,i 1))))
                 (set! ,i (+ ,i 2))))
              (draw-pixels-and-forward! ,x ,image-buffer ,offset ,width ,bit-depth ,val (lambda (,v ,r ,g ,b ,a) ,@body))
              (unless (< ,x ,width)
                (break)))
             (set! ,offset (+ ,offset (* ,width 4))))))))
   ((_ image-buffer width height bit-depth buffer ('lambda args body ...))
    (let ((x (gensym "x"))
          (y (gensym "y"))
          (i (gensym "i"))
          (vals (drop-right args 4))
          (offset (gensym "offset"))
          (idat-data (gensym "idat"))
          (idat-length (gensym "idatlen")))
      `(begin
         (unless (SCM_UVECTORP buffer)
           (Scm_Error "buffer must be <u8vector>, but got %S" buffer))
         (let* ((,offset :: int 0)
                (,x :: int 0)
                (,y :: int 0)
                (,i :: int 0)
                ,@(map (^v `(,v :: int 0)) vals)
                (,idat-data :: |unsigned char*| (SCM_U8VECTOR_ELEMENTS ,buffer))
                (,idat-length :: int (SCM_U8VECTOR_SIZE ,buffer)))
           (dotimes (,y ,height)
             (set! ,x 0)
             (loop
              (unless (< ,i ,idat-length)
                (Scm_Error "idat data is too little to fill %Ax%A image" ,width ,height))
              (case ,bit-depth
                ((8)
                 ,@(map (^(val) `(set! ,val (aref ,idat-data (post++ ,i)))) vals))
                ((16)
                 ,@(map (^(val)
                          `(begin
                             (set! ,val (logior (<< (aref ,idat-data ,i) 8)
                                                (aref ,idat-data (+ ,i 1))))
                             (set! ,i (+ ,i 2))))
                        vals)))
              (draw-pixels-and-forward! ,x ,image-buffer ,offset ,width ,bit-depth (lambda ,args ,@body))
              (unless (< ,x ,width)
                (break)))
             (set! ,offset (+ ,offset (* ,width 4)))))))))

 (define-cproc convert-from-gray! (image-buffer::<u8vector>
                                   width::<int>
                                   height::<int>
                                   bit-depth::<int>
                                   buffer
                                   trns-gray-sample::<int>)
   ::<void>
   (draw-image-buffer-from-idat image-buffer width height bit-depth buffer
                                (lambda (v r g b a)
                                  (let* ((l::int 0))
                                    (case bit-depth
                                      ((1) (set! l (* v 255)))
                                      ((2) (set! l (* v 85)))
                                      ((4) (set! l (* v 17))))
                                    (set! r l)
                                    (set! g l)
                                    (set! b l)
                                    (if (== trns-gray-sample l)
                                        (set! a 0)
                                        (set! a 255))))))

 (define-cproc convert-from-gray-alpha! (image-buffer::<u8vector>
                                         width::<int>
                                         height::<int>
                                         bit-depth::<int>
                                         buffer)
   ::<void>
   (draw-image-buffer-from-idat image-buffer width height bit-depth buffer
                                (lambda (v0 v1 r g b a)
                                  (case bit-depth
                                    ((8)
                                     (set! r v0)
                                     (set! g v0)
                                     (set! b v0)
                                     (set! a v1))
                                    ((16)
                                     (set! r (>> v0 8))
                                     (set! g (>> v0 8))
                                     (set! b (>> v0 8))
                                     (set! a (>> v1 8)))))))

 ;; plte := <u32vector>: index -> rgb
 ;; trns := <u8vector>: index -> alpha
 (define-cproc convert-from-index! (image-buffer::<u8vector>
                                    width::<int>
                                    height::<int>
                                    bit-depth::<int>
                                    buffer
                                    plte::<u32vector>
                                    trns::<u8vector>)
   ::<void>
   (draw-image-buffer-from-idat image-buffer width height bit-depth buffer
                                (lambda (v r g b a)
                                  (let* ((rgb::int (SCM_U32VECTOR_ELEMENT plte v)))
                                    (set! r (>> rgb 16))
                                    (set! g (logand (>> rgb 8) #xff))
                                    (set! b (logand rgb #xff))
                                    (set! a (SCM_U8VECTOR_ELEMENT trns v))))))

 ;; trns := #(r g b) | #f
 (define-cproc convert-from-true! (image-buffer::<u8vector>
                                   width::<int>
                                   height::<int>
                                   bit-depth::<int>
                                   buffer
                                   trns::<vector>)
   ::<void>
   (let* ((trns-r::int (Scm_GetInteger32 (SCM_VECTOR_ELEMENT trns 0)))
          (trns-g::int (Scm_GetInteger32 (SCM_VECTOR_ELEMENT trns 1)))
          (trns-b::int (Scm_GetInteger32 (SCM_VECTOR_ELEMENT trns 2))))
     (draw-image-buffer-from-idat image-buffer width height bit-depth buffer
                                  (lambda (vr vg vb r g b a)
                                    (case bit-depth
                                      ((8)
                                       (set! r vr)
                                       (set! g vg)
                                       (set! b vb))
                                      ((16)
                                       (set! r (>> vr 8))
                                       (set! g (>> vg 8))
                                       (set! b (>> vb 8))))
                                    (if (and (== trns-r vr)
                                             (== trns-g vg)
                                             (== trns-b vb))
                                        (set! a 0)
                                        (set! a 255))))))

 (define-cproc convert-from-true-alpha! (image-buffer::<u8vector>
                                         width::<int>
                                         height::<int>
                                         bit-depth::<int>
                                         buffer)
   ::<void>
   (draw-image-buffer-from-idat image-buffer width height bit-depth buffer
                                (lambda (vr vg vb va r g b a)
                                  (case bit-depth
                                    ((8)
                                     (set! r vr)
                                     (set! g vg)
                                     (set! b vb)
                                     (set! a va))
                                    ((16)
                                     (set! r (>> vr 8))
                                     (set! g (>> vg 8))
                                     (set! b (>> vb 8))
                                     (set! a (>> va 8)))))))

 (define-cfn compose-rgb (r::uint8_t g::uint8_t b::uint8_t) ::uint32_t :static :inline
   (return (logior (<< r 16) (<< g 8) b)))

 (define-cvar kColorTableSize::int :static 367)

 (define-cfn add-color-table (color-table::int32_t* r::uint8_t g::uint8_t b::uint8_t index::int*) ::bool :static :inline
   (let* ((rgb::uint32_t (compose-rgb r g b))
          ;; TODO: Consider better hash function.
          (h::int (logxor r g b))
          (j::int 0))
     (for ((set! j 0) (< j kColorTableSize) (inc! j))
       (let* ((i::int (% (+ h j) kColorTableSize))
              (v::uint32_t (aref color-table i)))
         (cond
           ((== v rgb)
            (set! (* index) i)
            (return false))
           ((== v -1)
            (set! (aref color-table i) rgb)
            (set! (* index) i)
            (return true))
           (else
            (continue)))))
     (Scm_Error "too many colors")
     ;; never reach
     (return false)))

 ;; Returns true if no conflict of transparency, otherwise returns false.
 (define-cfn add-trns-table (trns-table::int32_t* index::int a::uint8_t) ::bool :static :inline
   (let* ((v::int32_t (aref trns-table index)))
     (cond
       ((== v -1)
        (set! (aref trns-table index) a)
        (return true))
       ((== v a)
        (return true))
       (else
        (return false)))))

 (define-cfn %analyze-color (buffer::uint8_t*
                             width::int
                             height::int

                             color-type::int*
                             color-table::int32_t*
                             trns-table::int32_t*
                             has-alpha?::bool*
                             num-colors::int*)
   ::void :static :inline
   (let* ((i::int 0))
     (set! (* has-alpha?) false)
     (for ((= i 0) (< i (* width height 4)) (+= i 4))
       (let* ((r::uint8_t (aref buffer i))
              (g::uint8_t (aref buffer (+ i 1)))
              (b::uint8_t (aref buffer (+ i 2)))
              (a::uint8_t (aref buffer (+ i 3)))
              (color-index::int 0))
         (when (add-color-table color-table r g b (& color-index))
           (inc! (* num-colors)))
         (when (!= a 255)
           (set! (* has-alpha?) true))
         (cond
           ((and (< 256 (* num-colors)) (* has-alpha?))
            (set! (* color-type) PNG-COLOR-TRUE-ALPHA)
            (return))
           ((< 256 (* num-colors))
            (set! (* color-type) PNG-COLOR-TRUE)
            (return))
           ((not (add-trns-table trns-table color-index a))
            (set! (* color-type) PNG-COLOR-TRUE-ALPHA)
            (return))
           (else
            (continue)))))
     (set! (* color-type) PNG-COLOR-INDEX)
     (return)))

 ;; Returns color-type, inverse-palette (key is rgb) and trns-table (u8vector).
 ;; If the color-type isn't PNG-COLOR-INDEX, both inverse-palette and trns-table will be #f.
 (define-cproc analyze-color (image-buffer::<u8vector> width::<int> height::<int>) ::(<int> <top> <top>)
   (let* ((color-type::int -1)
          (has-alpha?::bool)
          (color-table::(.array int32_t [kColorTableSize]))
          (trns-table::(.array int32_t [kColorTableSize]))
          (num-colors::int 0)
          (i::int 0))
     (for ((= i 0) (< i kColorTableSize) (inc! i))
       (set! (aref color-table i) -1)
       (set! (aref trns-table i) -1))
     (%analyze-color (SCM_U8VECTOR_ELEMENTS image-buffer) width height
                     (& color-type) color-table trns-table (& has-alpha?) (& num-colors))
     (cond
       ((or (== color-type PNG-COLOR-TRUE-ALPHA)
            (== color-type PNG-COLOR-TRUE))
        (return color-type SCM_FALSE SCM_FALSE))
       ((== color-type PNG-COLOR-INDEX)
        (let* ((inv-plte (Scm_MakeHashTableSimple SCM_HASH_EQ kColorTableSize))
               (trns (?: has-alpha? (Scm_MakeU8Vector num-colors 255) SCM_FALSE))
               (j::int 0))
          (for ((= i 0) (< i kColorTableSize) (inc! i))
            (when (!= (aref color-table i) -1)
              (Scm_HashTableSet (SCM_HASH_TABLE inv-plte) (SCM_MAKE_INT (aref color-table i)) (SCM_MAKE_INT j) 0)
              (unless (SCM_FALSEP trns)
                (set! (SCM_U8VECTOR_ELEMENT trns j) (aref trns-table i)))
              (inc! j)))
          (return color-type inv-plte trns)))
       (else
        (Scm_Error "Invalid color-type: %A" (SCM_MAKE_INT color-type))))))

 ;; Returns bit-depth, data buffer, line-length.
 (define-cproc convert-index-color (image-buffer::<u8vector> width::<int> height::<int> inv-plte::<hash-table>)
   ::(<int> <u8vector> <int>)
   (let* ((i::int 0)
          (j::int 0)
          (y::int 0)
          (x::int 0)
          (v::uint8_t 0)
          (num-colors::int (Scm_HashCoreNumEntries (SCM_HASH_TABLE_CORE inv-plte))))
     (cond
       ((<= num-colors 2)
        (let* ((line-len::int (/ (+ width 7) 8))
               (buf (Scm_MakeU8Vector (* line-len height) 0)))
          (for ((= y 0) (< y height) (inc! y))
            (set! x 0)
            (while (< x width)
              (let* ((r::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer i))
                     (g::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 1)))
                     (b::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 2)))
                     (index::int (SCM_INT_VALUE (Scm_HashTableRef (SCM_HASH_TABLE inv-plte)
                                                                  (SCM_MAKE_INT (compose-rgb r g b))
                                                                  (SCM_MAKE_INT -1)))))
                (assert (<= 0 index))
                (set! v (logior (<< v 1) index))
                (inc! x)
                (set! i (+ i 4))
                (cond
                  ((== (% x 8) 0)
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  ((== x width)
                   (set! v (<< v (- 8 (% x 8))))
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  (else
                   (continue))))))
          (return 1 (SCM_U8VECTOR buf) line-len)))
       ((<= num-colors 4)
        (let* ((line-len::int (/ (+ width 3) 4))
               (buf (Scm_MakeU8Vector (* line-len height) 0)))
          (for ((= y 0) (< y height) (inc! y))
            (set! x 0)
            (while (< x width)
              (let* ((r::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer i))
                     (g::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 1)))
                     (b::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 2)))
                     (index::int (SCM_INT_VALUE (Scm_HashTableRef (SCM_HASH_TABLE inv-plte)
                                                                  (SCM_MAKE_INT (compose-rgb r g b))
                                                                  (SCM_MAKE_INT -1)))))
                (assert (<= 0 index))
                (set! v (logior (<< v 2) index))
                (inc! x)
                (set! i (+ i 4))
                (cond
                  ((== (% x 4) 0)
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  ((== x width)
                   (set! v (<< v (* 2 (- 4 (% x 4)))))
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  (else
                   (continue))))))
          (return 2 (SCM_U8VECTOR buf) line-len)))
       ((<= num-colors 16)
        (let* ((line-len::int (/ (+ width 1) 2))
               (buf (Scm_MakeU8Vector (* line-len height) 0)))
          (for ((= y 0) (< y height) (inc! y))
            (set! x 0)
            (while (< x width)
              (let* ((r::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer i))
                     (g::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 1)))
                     (b::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 2)))
                     (index::int (SCM_INT_VALUE (Scm_HashTableRef (SCM_HASH_TABLE inv-plte)
                                                                  (SCM_MAKE_INT (compose-rgb r g b))
                                                                  (SCM_MAKE_INT -1)))))
                (assert (<= 0 index))
                (set! v (logior (<< v 4) index))
                (inc! x)
                (set! i (+ i 4))
                (cond
                  ((== (% x 2) 0)
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  ((== x width)
                   (set! v (<< v (* 4 (- 2 (% x 2)))))
                   (set! (SCM_U8VECTOR_ELEMENT buf j) v)
                   (inc! j)
                   (set! v 0))
                  (else
                   (continue))))))
          (return 4 (SCM_U8VECTOR buf) line-len)))
       (else
        (let* ((buf (Scm_MakeU8Vector (* width height) 0)))
          (for ((= y 0) (< y height) (inc! y))
            (set! x 0)
            (while (< x width)
              (let* ((r::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer i))
                     (g::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 1)))
                     (b::uint8_t (SCM_U8VECTOR_ELEMENT image-buffer (+ i 2)))
                     (index::int (SCM_INT_VALUE (Scm_HashTableRef (SCM_HASH_TABLE inv-plte)
                                                                  (SCM_MAKE_INT (compose-rgb r g b))
                                                                  (SCM_MAKE_INT -1)))))
                (assert (<= 0 index))
                (inc! x)
                (set! i (+ i 4))
                (set! (SCM_U8VECTOR_ELEMENT buf j) index)
                (inc! j))))
          (return 8 (SCM_U8VECTOR buf) width))))))

 ;; Returns bit-depth, data buffer and line-length.
 (define-cproc convert-rgb-color (image-buffer::<u8vector> width::<int> height::<int>) ::(<int> <u8vector> <int>)
   (let* ((i::int 0)
          (j::int 0)
          (x::int 0)
          (y::int 0)
          (buf (Scm_MakeU8Vector (* width height 3) 0)))
     (for ((= y 0) (< y height) (inc! y))
       (for ((= x 0) (< x width) (inc! x))
         ;; Copy R
         (set! (SCM_U8VECTOR_ELEMENT buf (post++ j)) (SCM_U8VECTOR_ELEMENT image-buffer (post++ i)))
         ;; Copy G
         (set! (SCM_U8VECTOR_ELEMENT buf (post++ j)) (SCM_U8VECTOR_ELEMENT image-buffer (post++ i)))
         ;; Copy B
         (set! (SCM_U8VECTOR_ELEMENT buf (post++ j)) (SCM_U8VECTOR_ELEMENT image-buffer (post++ i)))
         ;; Skip A
         (inc! i)))
     (return 8 (SCM_U8VECTOR buf) (* width 3))))

 )
