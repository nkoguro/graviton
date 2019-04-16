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

(select-module graviton.video)

(inline-stub
 (.include "SDL.h"
           "gauche.h"
           "gauche/extend.h"
           "graviton.h"
           "stdbool.h")

 (define-cvar make-image :static SCM_UNDEFINED)
 (define-cvar make-sprite :static SCM_UNDEFINED)

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
           (unless (GRV_TILE_IMAGEP tile)
             (Scm_Error "<graviton-tile-image> required, but got %S" tile))
           (set! gtile (GRV_TILE_IMAGE_PTR tile))
           (Grv_Bitblt (GRV_IMAGE_PTR (-> gtile image))
                       (& (-> gtile rect))
                       gimage
                       (& dstrect)
                       fg-color))))))
 ) ;; end of inline-stub

(include "types.scm")

(define-cproc make-tile-map (window
                             tile-images::<vector>
                             columns::<int>
                             rows::<int>
                             x::<double>
                             y::<double>
                             :key
                             (z::<double> 0)
                             (fill::<uint32> 0))
  ::<graviton-tile-map>
  (let* ((gtilemap::GrvTileMap* (SCM_NEW GrvTileMap))
         (size::int (* columns rows))
         (tile-width::int)
         (tile-height::int)
         (sprite))
    (when (== (SCM_VECTOR_SIZE tile-images) 0)
      (Scm_Error "tile-images must have at least one element"))
    (dotimes (i (SCM_VECTOR_SIZE tile-images))
      (unless (GRV_TILE_IMAGEP (Scm_VectorRef tile-images i SCM_UNBOUND))
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

    (SCM_BIND_PROC make-image "make-image" (SCM_MODULE Grv_GravitonVideoModule))
    (SCM_BIND_PROC make-sprite "make-sprite" (SCM_MODULE Grv_GravitonVideoModule))

    (set! (-> gtilemap columns) columns
          (-> gtilemap rows) rows
          (-> gtilemap offset) 0

          (-> gtilemap image) (Scm_ApplyRec2 make-image
                                             (SCM_MAKE_INT (* tile-width columns))
                                             (SCM_MAKE_INT (* tile-height rows)))
          (-> gtilemap tile-images) (SCM_OBJ tile-images)
          (-> gtilemap tile-width) tile-width
          (-> gtilemap tile-height) tile-height

          sprite (Scm_ApplyRec make-sprite
                               (Scm_List window
                                         ':image (-> gtilemap image)
                                         ':x (Scm_MakeFlonum x)
                                         ':y (Scm_MakeFlonum y)
                                         ':z (Scm_MakeFlonum z)
                                         NULL)))
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

