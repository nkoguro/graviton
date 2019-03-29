;;;
;;; png-lib.scm - PNG reader/writer
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

(select-module graviton.png)

(inline-stub
 (declcode
  (.include "assert.h"
            "stdbool.h"
            "stdlib.h"
            "string.h"
            "gauche.h"
            "gauche/hash.h"
            "gauche/vector.h"))

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
