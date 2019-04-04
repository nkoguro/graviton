;;;
;;; draw.scm - Vector graphics drawing
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

(select-module graviton.video)

(inline-stub
 (declcode
  (.include "SDL.h"
            "gauche.h"
            "graviton.h"
            "stdbool.h"
            "string.h"))

 (define-ctype ScratchArea::(.struct
                             (x::int
                              y::int
                              w::int
                              h::int
                              data::char*)))

 (define-cfn sign (a::int)
   ::int
   (cond
     ((< a 0)
      (return -1))
     ((== a 0)
      (return 0))
     (else
      (return 1))))

 (define-cfn create-scratch-area (x::int y::int w::int h::int)
   ::ScratchArea*
   (let* ((area::ScratchArea* (SCM_NEW (.type ScratchArea))))
     (set! (-> area x) x
           (-> area y) y
           (-> area w) w
           (-> area h) h
           (-> area data) (SCM_NEW_ATOMIC_ARRAY (.type char) (* w h)))
     (memset (-> area data) 0 (* w h))
     (return area)))

 "
typedef enum {
    SCRATCH_PIXEL_EMPTY = 0,
    SCRATCH_PIXEL_BORDER = 1,
    SCRATCH_PIXEL_OUTSIDE = 2,
} ScratchPixelType;
"
 (define-cfn scratch-fill-rect! (area::ScratchArea* rect::SDL_Rect*)
   ::void
   (let* ((x::int (-> rect x))
          (y::int (-> rect y))
          (w::int (-> rect w))
          (h::int (-> rect h))
          (i::int)
          (maxi::int (- (+ x w) (-> area x)))
          (j::int)
          (maxj::int (- (+ y h) (-> area y))))
     (for ((set! j (- y (-> area y))) (< j maxj) (pre++ j))
       (for ((set! i (- x (-> area x))) (< i maxi) (pre++ i))
         (set! (aref (-> area data) (+ (* j (-> area w)) i)) SCRATCH_PIXEL_BORDER)))))

 (define-cfn scratch-fill-border! (area::ScratchArea*)
   (let* ((idxs SCM_NIL)
          (w::int (-> area w))
          (h::int (-> area h)))
     (let* ((x::int))
       (for ((set! x 0) (< x w) (pre++ x))
         (let* ((t::int x))
           (when (== (aref (-> area data) t) SCRATCH_PIXEL_EMPTY)
             (set! (aref (-> area data) t) SCRATCH_PIXEL_OUTSIDE
                   idxs (Scm_Cons (SCM_MAKE_INT (+ t w)) idxs))))
         (let* ((b::int (+ (* (- h 1) w) x)))
           (when (== (aref (-> area data) b) SCRATCH_PIXEL_EMPTY)
             (set! (aref (-> area data) b) SCRATCH_PIXEL_OUTSIDE
                   idxs (Scm_Cons (SCM_MAKE_INT (- b w)) idxs))))))
     (let* ((y::int))
       (for ((set! y 0) (< y h) (pre++ y))
         (let* ((l::int (* y w)))
           (when (== (aref (-> area data) l) SCRATCH_PIXEL_EMPTY)
             (set! (aref (-> area data) l) SCRATCH_PIXEL_OUTSIDE
                   idxs (Scm_Cons (SCM_MAKE_INT (+ l 1)) idxs))))
         (let* ((r::int (- (* (+ y 1) w) 1)))
           (when (== (aref (-> area data) r) SCRATCH_PIXEL_EMPTY)
             (set! (aref (-> area data) r) SCRATCH_PIXEL_OUTSIDE
                   idxs (Scm_Cons (SCM_MAKE_INT (- r 1)) idxs))))))
     (while (not (SCM_NULLP idxs))
       (let* ((i::int (SCM_INT_VALUE (SCM_CAR idxs))))
         (set! idxs (SCM_CDR idxs))
         (cond
           ((!= (aref (-> area data) i) SCRATCH_PIXEL_EMPTY)
            (continue))
           (else
            (set! (aref (-> area data) i) SCRATCH_PIXEL_OUTSIDE
                  idxs (Scm_Cons (SCM_MAKE_INT (- i 1)) idxs)
                  idxs (Scm_Cons (SCM_MAKE_INT (+ i 1)) idxs)
                  idxs (Scm_Cons (SCM_MAKE_INT (- i w)) idxs)
                  idxs (Scm_Cons (SCM_MAKE_INT (+ i w)) idxs))))))))

 (define-cfn fill-inside (gimage::GrvImage* area::ScratchArea* color::Uint32)
   ::void
   (let* ((sx::int)
          (x::int)
          (y::int)
          (w::int (-> area w))
          (h::int (-> area h)))
     (for ((set! y 0) (< y h) (pre++ y))
       (set! sx -1)
       (for ((set! x 0) (< x w) (pre++ x))
         (let* ((i::int (+ (* y w) x)))
           (cond
             ((< sx 0)
              (when (!= (aref (-> area data) i) SCRATCH_PIXEL_OUTSIDE)
                (set! sx x)))
             (else
              (when (== (aref (-> area data) i) SCRATCH_PIXEL_OUTSIDE)
                (let* ((rect::SDL_Rect))
                  (set! (ref rect x) (+ sx (-> area x))
                        (ref rect y) (+ y (-> area y))
                        (ref rect w) (- x sx)
                        (ref rect h) 1)
                  (when (!= (SDL_FillRect (-> gimage surface) (& rect) color) 0)
                    (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError))))
                (set! sx -1))))))
       (unless (< sx 0)
         (let* ((rect::SDL_Rect))
           (set! (ref rect x) (+ sx (-> area x))
                 (ref rect y) (+ y (-> area y))
                 (ref rect w) (- w sx)
                 (ref rect h) 1)
           (when (!= (SDL_FillRect (-> gimage surface) (& rect) color) 0)
             (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))))))
   (Grv_SetNeedsRefreshImage gimage (-> area x) (-> area y) (-> area w) (-> area h)))

 (define-cfn fill-rect (gimage::GrvImage* x0::int y0::int x1::int y1::int color::Uint32)
   ::void
   (let* ((rect::SDL_Rect))
     (set! (ref rect x) (?: (< x0 x1) x0 x1)
           (ref rect y) (?: (< y0 y1) y0 y1)
           (ref rect w) (+ (abs (- x0 x1)) 1)
           (ref rect h) (+ (abs (- y0 y1)) 1))
     (when (!= (SDL_FillRect (-> gimage surface) (& rect) color) 0)
       (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))
     (Grv_SetNeedsRefreshImage gimage (ref rect x) (ref rect y) (ref rect w) (ref rect h))))

 (define-cfn %%draw-line (gimage::GrvImage* x0::int y0::int x1::int y1::int color::Uint32 area::ScratchArea*)
   ::void
   (let* ((surface::SDL_Surface* (-> gimage surface))
          (lx::int (+ (abs (- x0 x1)) 1))
          (ly::int (+ (abs (- y0 y1)) 1)))
     (cond
       ((<= lx ly)
        (let* ((x::int (?: (< y0 y1) x0 x1))
               (end-x::int (?: (< y0 y1) x1 x0))
               (dx::int (sign (- end-x x)))
               (y::int (?: (< y0 y1) y0 y1))
               (end-y::int (?: (< y0 y1) y1 y0))
               (dy::double (/ (cast double ly) (cast double lx)))
               (ay::double (+ y dy)))
          (loop
           (let* ((rect::SDL_Rect))
             (set! (ref rect x) x
                   (ref rect y) y
                   (ref rect w) 1
                   (ref rect h) (?: (== x end-x)
                                    (+ (- end-y y) 1)
                                    (cast int (round (- ay y)))))
             (cond
               (area
                (scratch-fill-rect! area (& rect)))
               (else
                (when (!= (SDL_FillRect surface (& rect) color) 0)
                  (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))))
             (when (== x end-x)
               (break))
             (set! x (+ x dx)
                   y (+ y (ref rect h))
                   ay (+ ay dy))))))
       (else
        (let* ((x::int (?: (< x0 x1) x0 x1))
               (end-x::int (?: (< x0 x1) x1 x0))
               (dx::double (/ (cast double lx) (cast double ly)))
               (ax::double (+ x dx))
               (y::int (?: (< x0 x1) y0 y1))
               (end-y::int (?: (< x0 x1) y1 y0))
               (dy::int (sign (- end-y y))))
          (loop
           (let* ((rect::SDL_Rect))
             (set! (ref rect x) x
                   (ref rect y) y
                   (ref rect w) (?: (== y end-y)
                                    (+ (- end-x x) 1)
                                    (cast int (round (- ax x))))
                   (ref rect h) 1)
             (cond
               (area
                (scratch-fill-rect! area (& rect)))
               (else
                (when (!= (SDL_FillRect surface (& rect) color) 0)
                  (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))))
             (when (== y end-y)
               (break))
             (set! x (+ x (ref rect w))
                   y (+ y dy)
                   ax (+ ax dx))))))))
   (Grv_SetNeedsRefreshImage gimage (?: (< x0 x1) x0 x1) (?: (< y0 y1) y0 y1) (+ (abs (- x0 x1)) 1) (+ (abs (- y0 y1)) 1)))
 )  ;; end of inline-stub

(include "types.scm")

(define-cproc %draw-rect (gimage::<graviton-image> x::<double> y::<double> w::<double> h::<double> color::<int> fill?::<boolean>)
  ::<void>
  (let* ((x0::double x)
         (y0::double y)
         (x1::double (+ x w))
         (y1::double (+ y h))
         (ix0::int)
         (iy0::int)
         (ix1::int)
         (iy1::int))
    (Grv_ComputeImageCoordinate gimage x0 y0 (& ix0) (& iy0))
    (Grv_ComputeImageCoordinate gimage x1 y1 (& ix1) (& iy1))
    (set! ix1 (- ix1 1)
          iy1 (- iy1 1))
    (cond
      (fill?
       (fill-rect gimage ix0 iy0 ix1 iy1 color))
      (else
       (%%draw-line gimage ix0 iy0 ix1 iy0 color NULL)
       (%%draw-line gimage ix1 iy0 ix1 iy1 color NULL)
       (%%draw-line gimage ix1 iy1 ix0 iy1 color NULL)
       (%%draw-line gimage ix0 iy1 ix0 iy0 color NULL)))))

(define-cproc %draw-line (gimage::<graviton-image> points::<list> color::<int>)
  ::<void>
  (cond
    ((SCM_NULLP points)
     (return))
    ((SCM_NULLP (SCM_CDR points))
     (let* ((x::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 0 SCM_UNBOUND)))
            (y::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 1 SCM_UNBOUND)))
            (ix::int)
            (iy::int))
       (Grv_ComputeImageCoordinate gimage x y (& ix) (& iy))
       (%%draw-line gimage ix iy ix iy color NULL)))
    (else
     (let* ((x0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 0 SCM_UNBOUND)))
            (y0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 1 SCM_UNBOUND)))
            (ix0::int)
            (iy0::int))
       (Grv_ComputeImageCoordinate gimage x0 y0 (& ix0) (& iy0))
       (for-each (lambda (point)
                   (let* ((x1::double (Scm_GetDouble (Scm_ListRef point 0 SCM_UNBOUND)))
                          (y1::double (Scm_GetDouble (Scm_ListRef point 1 SCM_UNBOUND)))
                          (ix1::int)
                          (iy1::int))
                     (Grv_ComputeImageCoordinate gimage x1 y1 (& ix1) (& iy1))
                     (%%draw-line gimage ix0 iy0 ix1 iy1 color NULL)
                     (set! ix0 ix1
                           iy0 iy1)))
                 (SCM_CDR points))))))

(define-cproc %draw-polygon (gimage::<graviton-image> points::<list> color::<int> fill?::<boolean>)
  ::<void>
  (let* ((num-points::int (Scm_Length points)))
    (case num-points
      ((0)
       (return))
      ((1)
       (let* ((x::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 0 SCM_UNBOUND) 0 SCM_UNBOUND)))
              (y::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 0 SCM_UNBOUND) 1 SCM_UNBOUND)))
              (ix::int)
              (iy::int))
         (Grv_ComputeImageCoordinate gimage x y (& ix) (& iy))
         (%%draw-line gimage ix iy ix iy color NULL)))
      ((2)
       (let* ((x0::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 0 SCM_UNBOUND) 0 SCM_UNBOUND)))
              (y0::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 0 SCM_UNBOUND) 1 SCM_UNBOUND)))
              (x1::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 1 SCM_UNBOUND) 0 SCM_UNBOUND)))
              (y1::double (Scm_GetDouble (Scm_ListRef (Scm_ListRef points 1 SCM_UNBOUND) 1 SCM_UNBOUND)))
              (ix0::int)
              (iy0::int)
              (ix1::int)
              (iy1::int))
         (Grv_ComputeImageCoordinate gimage x0 y0 (& ix0) (& iy0))
         (Grv_ComputeImageCoordinate gimage x1 y1 (& ix1) (& iy1))
         (%%draw-line gimage ix0 iy0 ix1 iy1 color NULL)))
      (else
       (let* ((x0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 0 SCM_UNBOUND)))
              (y0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 1 SCM_UNBOUND)))
              (ix0::int)
              (iy0::int)
              (sx::int)
              (sy::int)
              (ex::int)
              (ey::int)
              (area::ScratchArea* NULL)
              (nodes SCM_NIL))
         (Grv_ComputeImageCoordinate gimage x0 y0 (& ix0) (& iy0))
         (set! nodes (SCM_LIST1 (Scm_Cons (SCM_MAKE_INT ix0) (SCM_MAKE_INT iy0))))
         (set! sx ix0
               sy iy0
               ex ix0
               ey iy0)
         (for-each (lambda (point)
                     (let* ((x::double (Scm_GetDouble (Scm_ListRef point 0 SCM_UNBOUND)))
                            (y::double (Scm_GetDouble (Scm_ListRef point 1 SCM_UNBOUND)))
                            (ix::int)
                            (iy::int))
                       (Grv_ComputeImageCoordinate gimage x y (& ix) (& iy))
                       (set! nodes (Scm_Cons (Scm_Cons (SCM_MAKE_INT ix) (SCM_MAKE_INT iy)) nodes))
                       (when (< ix sx)
                         (set! sx ix))
                       (when (< iy sy)
                         (set! sy iy))
                       (when (< ex ix)
                         (set! ex ix))
                       (when (< ey iy)
                         (set! ey iy))))
                   (SCM_CDR points))
         (when fill?
           (set! area (create-scratch-area sx sy (+ (- ex sx) 1) (+ (- ey sy) 1))))
         (for-each (lambda (node)
                     (let* ((ix1::int (SCM_INT_VALUE (SCM_CAR node)))
                            (iy1::int (SCM_INT_VALUE (SCM_CDR node))))
                       (%%draw-line gimage ix0 iy0 ix1 iy1 color area)
                       (set! ix0 ix1
                             iy0 iy1)))
                   nodes)
         (when fill?
           (scratch-fill-border! area)
           (fill-inside gimage area color)))))))
