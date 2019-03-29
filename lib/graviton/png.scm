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

(define-module graviton.png
  (use binary.io)
  (use gauche.uvector)
  (use rfc.zlib)

  (export read-png-image
          write-png-image

          PNG-FILTER-NONE
          PNG-FILTER-SUB
          PNG-FILTER-UP
          PNG-FILTER-AVERAGE
          PNG-FILTER-PAETH))

(select-module graviton.png)
(dynamic-load "graviton-png")

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
