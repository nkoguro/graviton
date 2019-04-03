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
  (use gauche.uvector)
  (use graviton.png)

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

(set! (setter window-fullscreen?) set-window-fullscreen!)
(set! (setter window-position) set-window-position!)
(set! (setter window-title) set-window-title!)
(set! (setter window-resizable?) set-window-resizable!)
(set! (setter window-icon) set-window-icon!)
(set! (setter window-maximized?) set-window-maximized!)
(set! (setter window-minimized?) set-window-minimized!)
