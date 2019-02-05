;;;
;;; color.scm - Color utility
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

(define-module graviton.color
  (use gauche.threads)

  (export rgb
          rgba
          color

          register-color!
          define-color))

(select-module graviton.color)

(define (rgb r g b)
  (rgba r g b #xff))

(define (rgba r g b a)
  (if (eq? (native-endian) 'little-endian)
      (logior r
              (ash g 8)
              (ash b 16)
              (ash a 24))
      (logior a
              (ash b 8)
              (ash g 16)
              (ash r 24))))

(define *color-table*
  (atom
    `((white   . ,(rgb #xff #xff #xff))
      (silver  . ,(rgb #xc0 #xc0 #xc0))
      (gray    . ,(rgb #x80 #x80 #x80))
      (black   . ,(rgb #x00 #x00 #x00))
      (red     . ,(rgb #xff #x00 #x00))
      (maroon  . ,(rgb #x80 #x00 #x00))
      (yellow  . ,(rgb #xff #xff #x00))
      (olive   . ,(rgb #x80 #x80 #x00))
      (lime    . ,(rgb #x00 #xff #x00))
      (green   . ,(rgb #x00 #x80 #x00))
      (aqua    . ,(rgb #x00 #xff #xff))
      (teal    . ,(rgb #x00 #x80 #x80))
      (blue    . ,(rgb #x00 #x00 #xff))
      (navy    . ,(rgb #x00 #x00 #x80))
      (fuchsia . ,(rgb #xff #x00 #xff))
      (purple  . ,(rgb #x80 #x00 #x80)))))

(define (color name :optional (a #xff))
  (atomic *color-table*
    (lambda (color-alist)
      (let1 v (assq-ref color-alist name)
        (unless v
          (errorf "color not found: ~a" name))
        (cond
          ((= a #xff)
           v)
          ((eq? (native-endian) 'little-endian)
           (logior (ash a 24)
                   (logand v #x00ffffff)))
          (else
           (logior a
                   (logand #xffffff00))))))))

(define (register-color! name r g b :optional (a #xff))
  (atomic-update! *color-table*
    (lambda (color-alist)
      (assoc-set! color-alist name (rgba r g b a)))))

(define-syntax define-color
  (syntax-rules ()
    ((_ name r g b)
     (register-color! 'name r g b))
    ((_ name r g b a)
     (register-color! 'name r g b a))))


