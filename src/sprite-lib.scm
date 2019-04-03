;;;
;;; sprite.scm - Sprite
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
            "gauche/extend.h"
            "graviton.h"
            "stdbool.h"
            ))

 (define-cfn finalize-sprite (z data::void*)
   ::void
   (when (GRV_SPRITEP z)
     (set! (-> (GRV_SPRITE_PTR z) window) SCM_FALSE
           (-> (GRV_SPRITE_PTR z) image) SCM_FALSE)))

 (define-cfn Grv_InvalidateSprite (gsprite::GrvSprite*)
   ::void
   (unless (SCM_FALSEP (-> gsprite image))
     (Grv_ReleaseTexture (-> gsprite window) (GRV_IMAGE_PTR (-> gsprite image))))
   (set! (-> gsprite window) SCM_FALSE))

 (define-cfn compute-sprite-actual-params (gsprite::GrvSprite* angle-deg::double* zoom-x::double* zoom-y::double* flip::SDL_RendererFlip*)
   ::void
   (let* ((deg::double (/ (* 180 (-> gsprite angle)) M_PI)))
     (cond
       ((and (< (-> gsprite zoom-x) 0) (< (-> gsprite zoom-y) 0))
        (set! (* angle-deg) (+ deg 180)
              (* zoom-x) (- (-> gsprite zoom-x))
              (* zoom-y) (- (-> gsprite zoom-y))
              (* flip) SDL_FLIP_NONE))
       ((< zoom-x 0)
        (set! (* angle-deg) deg
              (* zoom-x) (- (-> gsprite zoom-x))
              (* zoom-y) (-> gsprite zoom-y)
              (* flip) SDL_FLIP_VERTICAL))
       ((< zoom-y 0)
        (set! (* angle-deg) deg
              (* zoom-x) (-> gsprite zoom-x)
              (* zoom-y) (- (-> gsprite zoom-y))
              (* flip) SDL_FLIP_HORIZONTAL))
       (else
        (set! (* angle-deg) deg
              (* zoom-x) (-> gsprite zoom-x)
              (* zoom-y) (-> gsprite zoom-y)
              (* flip) SDL_FLIP_NONE)))))

 (define-cfn Grv_RenderSprite (gsprite::GrvSprite*)
   ::void
   (when (or (SCM_FALSEP (-> gsprite image))
             (not (-> gsprite visible)))
     (return))
   (let* ((gimage::GrvImage* (GRV_IMAGE_PTR (-> gsprite image))))
     (Grv_RefreshTextures gimage)
     (let* ((angle-deg::double)
            (zoom-x::double)
            (zoom-y::double)
            (flip::SDL_RendererFlip))
       (compute-sprite-actual-params gsprite (& angle-deg) (& zoom-x) (& zoom-y) (& flip))
       (let* ((win (-> gsprite window))
              (gwin::GrvWindow* (GRV_WINDOW_PTR win))
              (texture::SDL_Texture* (Grv_RetrieveTexture win gimage))
              (spr-w::double (* (-> gsprite srcrect w) zoom-x))
              (spr-h::double (* (-> gsprite srcrect h) zoom-y))
              (dstrect::SDL_Rect)
              (r::Uint8)
              (g::Uint8)
              (b::Uint8)
              (a::Uint8))
         (Grv_DecomposeRGBA (-> gsprite color) (& r) (& g) (& b) (& a))
         (set! (ref dstrect x) (cast int (round (- (+ (-> gsprite center-x) (-> gwin offset-x)) (/ spr-w 2.0))))
               (ref dstrect y) (cast int (round (- (+ (-> gsprite center-y) (-> gwin offset-y)) (/ spr-h 2.0))))
               (ref dstrect w) (cast int (round spr-w))
               (ref dstrect h) (cast int (round spr-h)))
         (when (< (SDL_SetTextureAlphaMod texture a) 0)
           (Scm_Error "SDL_SetTextureAlphaMod failed: %s" (SDL_GetError)))
         (when (< (SDL_SetTextureColorMod texture r g b) 0)
           (Scm_Error "SDL_SetTextureColorMod failed: %s" (SDL_GetError)))
         (when (< (SDL_SetTextureBlendMode texture SDL_BLENDMODE_BLEND) 0)
           (Scm_Error "SDL_SetTextureBlendMode failed: %s" (SDL_GetError)))
         (let* ((clip::SDL_Rect* NULL))
           (cond
             ((-> gsprite clip)
              (set! clip (-> gsprite clip)))
             ((-> gwin clip)
              (set! clip (-> gwin clip))))
           (when (< (SDL_RenderSetClipRect (-> gwin renderer) clip) 0)
             (Scm_Error "SDL_RenderSetClipRect failed: %s" (SDL_GetError))))
         (when (< (SDL_RenderCopyEx (-> gwin renderer)
                                    texture
                                    (-> gsprite srcrect)
                                    (& dstrect)
                                    angle-deg
                                    NULL
                                    flip) 0)
           (Scm_Error "SDL_RenderCopyEx failed: %s" (SDL_GetError)))))))

 (define-cfn remove-window-sprite (sprite)
   ::void
   (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR (-> (GRV_SPRITE_PTR sprite) window)))
          (sprites (-> gwin sprites)))
     (cond
       ((and (== (Scm_Length sprites) 1)
             (SCM_EQ (SCM_CAR sprites) sprite))
        (set! (-> gwin sprites) SCM_NIL))
       (else
        (let* ((prev-pair (SCM_CAR sprites)))
          (pair-for-each (lambda (pair)
                           (when (SCM_EQ (SCM_CAR pair) sprite)
                             (SCM_SET_CDR prev-pair (SCM_CDR pair))
                             (break))
                           (set! prev-pair pair))
                         (SCM_CDR sprites)))))))

 (define-cfn insert-window-sprite (sprite)
   ::void
   (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR (-> (GRV_SPRITE_PTR sprite) window)))
          (gsprite::GrvSprite* (GRV_SPRITE_PTR sprite)))
     (cond
       ((SCM_NULLP (-> gwin sprites))
        (set! (-> gwin sprites) (SCM_LIST1 sprite)))
       (else
        (pair-for-each (lambda (pair)
                         (cond
                           ((> (-> (GRV_SPRITE_PTR (SCM_CAR pair)) z) (-> gsprite z))
                            (SCM_SET_CDR pair (Scm_Cons (SCM_CAR pair) (SCM_CDR pair)))
                            (SCM_SET_CAR pair sprite)
                            (break))
                           ((SCM_NULLP (SCM_CDR pair))
                            (SCM_SET_CDR pair (Scm_Cons sprite SCM_NIL))
                            (break))))
                       (-> gwin sprites))))))

 (define-cfn make-sprite (window
                          sprite-image
                          center-x::double
                          center-y::double
                          z::double
                          srcrect::SDL_Rect*
                          angle::double
                          zoom-x::double
                          zoom-y::double
                          visible?::bool
                          color::Uint32)
   ::ScmObj
   (let* ((gsprite::GrvSprite* (SCM_NEW GrvSprite)))
     (unless (SCM_FALSEP sprite-image)
       (Grv_RetainTexture window (GRV_IMAGE_PTR sprite-image)))

     (set! (-> gsprite window) window
           (-> gsprite image) sprite-image
           (-> gsprite center-x) center-x
           (-> gsprite center-y) center-y
           (-> gsprite z) z
           (-> gsprite srcrect) srcrect
           (-> gsprite angle) angle
           (-> gsprite zoom-x) zoom-x
           (-> gsprite zoom-y) zoom-y
           (-> gsprite visible) visible?
           (-> gsprite color) color
           (-> gsprite clip) NULL)
     (let* ((sprite (GRV_SPRITE_BOX gsprite)))
       (Scm_RegisterFinalizer sprite finalize-sprite NULL)
       (insert-window-sprite sprite)
       (return sprite))))
 ) ;; end of inline-stub

(include "types.scm")

(define-cproc make-sprite (window
                           :key
                           (image #f)
                           (x::<double> 0.0)
                           (y::<double> 0.0)
                           (z::<double> 0.0)
                           (angle::<double> 0.0)
                           zoom
                           (visible?::<boolean> #t)
                           (color::<uint32> #xffffffff))
  (unless (GRV_WINDOWP window)
    (Scm_Error "window must be <graviton-window>, but got %S" window))
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGEP image)
              (GRV_TILE_IMAGEP image))
    (Scm_Error "image must be <graviton-image>, <graviton-tile-image> or #f, but got %S" image))
  (unless (or (SCM_UNBOUNDP zoom)
              (SCM_REALP zoom)
              (and (SCM_LISTP zoom)
                   (SCM_REALP (Scm_ListRef zoom 0 SCM_UNBOUND))
                   (SCM_REALP (Scm_ListRef zoom 1 SCM_UNBOUND))))
    (Scm_Error "zoom must be <real> or (<real> <real>), but got %S" zoom))

  (let* ((gsprite::GrvSprite* (SCM_NEW (.type GrvSprite)))
         (sprite-image SCM_FALSE)
         (zoom-x::double 1.0)
         (zoom-y::double 1.0)
         (srcrect::SDL_Rect* NULL))
    (cond
      ((SCM_REALP zoom)
       (set! zoom-x (Scm_GetDouble zoom)
             zoom-y (Scm_GetDouble zoom)))
      ((SCM_LISTP zoom)
       (set! zoom-x (Scm_GetDouble (Scm_ListRef zoom 0 SCM_UNBOUND))
             zoom-y (Scm_GetDouble (Scm_ListRef zoom 1 SCM_UNBOUND)))))

    (cond
      ((GRV_IMAGEP image)
       (set! sprite-image image
             srcrect (SCM_NEW SDL_Rect)
             (-> srcrect x) 0
             (-> srcrect y) 0
             (-> srcrect w) (-> (GRV_IMAGE_PTR image) surface w)
             (-> srcrect h) (-> (GRV_IMAGE_PTR image) surface h)))
      ((GRV_TILE_IMAGEP image)
       (set! sprite-image (-> (GRV_TILE_IMAGE_PTR image) image)
             srcrect (& (-> (GRV_TILE_IMAGE_PTR image) rect)))))

    (return (make-sprite window sprite-image x y z srcrect angle zoom-x zoom-y visible? color))))

(define-cproc set-sprite-image! (gsprite::<graviton-sprite> image)
  ::<void>
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGEP image)
              (GRV_TILE_IMAGEP image))
    (Scm_Error "image must be <graviton-image>, <graviton-tile-image> or #f, but got %S" image))

  (let* ((sprite-image SCM_FALSE)
         (srcrect::SDL_Rect* NULL))
    (cond
      ((GRV_IMAGEP image)
       (let* ((gimage::GrvImage* (GRV_IMAGE_PTR image)))
         (set! sprite-image image
               srcrect (SCM_NEW SDL_Rect)
               (-> srcrect x) 0
               (-> srcrect y) 0
               (-> srcrect w) (-> gimage surface w)
               (-> srcrect h) (-> gimage surface h))))
      ((GRV_TILE_IMAGEP image)
       (set! sprite-image (-> (GRV_TILE_IMAGE_PTR image) image)
             srcrect (& (-> (GRV_TILE_IMAGE_PTR image) rect)))))

    (unless (SCM_FALSEP sprite-image)
      (Grv_RetainTexture (-> gsprite window) (GRV_IMAGE_PTR sprite-image)))
    (unless (SCM_FALSEP (-> gsprite image))
      (Grv_ReleaseTexture (-> gsprite window) (GRV_IMAGE_PTR (-> gsprite image))))

    (set! (-> gsprite image) sprite-image
          (-> gsprite srcrect) srcrect)))

(define-cproc sprite-image (gsprite::<graviton-sprite>)
  ::<top>
  (return (-> gsprite image)))

(define-cproc set-sprite-x! (gsprite::<graviton-sprite> x::<double>)
  ::<void>
  (set! (-> gsprite center-x) x))

(define-cproc sprite-x (gsprite::<graviton-sprite>)
  ::<double>
  (return (-> gsprite center-x)))

(define-cproc set-sprite-y! (gsprite::<graviton-sprite> y::<double>)
  ::<void>
  (set! (-> gsprite center-y) y))

(define-cproc sprite-y (gsprite::<graviton-sprite>)
  ::<double>
  (return (-> gsprite center-y)))

(define-cproc set-sprite-z! (sprite z::<double>)
  ::<void>
  (unless (GRV_SPRITEP sprite)
    (Scm_Error "<graviton-sprite> required, but got %S" sprite))
  (remove-window-sprite sprite)
  (set! (-> (GRV_SPRITE_PTR sprite) z) z)
  (insert-window-sprite sprite))

(define-cproc sprite-z (gsprite::<graviton-sprite>)
  ::<double>
  (return (-> gsprite z)))

(define-cproc set-sprite-angle! (gsprite::<graviton-sprite> angle::<double>)
  ::<void>
  (set! (-> gsprite angle) angle))

(define-cproc sprite-angle (gsprite::<graviton-sprite>)
  ::<double>
  (return (-> gsprite angle)))

(define-cproc set-sprite-zoom! (gsprite::<graviton-sprite> zoom)
  ::<void>
  (let* ((zoom-x::double)
         (zoom-y::double))
    (cond
      ((SCM_REALP zoom)
       (let* ((z::double (Scm_GetDouble zoom)))
         (set! zoom-x z
               zoom-y z)))
      ((and (SCM_LISTP zoom)
            (SCM_REALP (Scm_ListRef zoom 0 SCM_UNBOUND))
            (SCM_REALP (Scm_ListRef zoom 1 SCM_UNBOUND)))
       (set! zoom-x (Scm_GetDouble (Scm_ListRef zoom 0 SCM_UNBOUND))
             zoom-y (Scm_GetDouble (Scm_ListRef zoom 1 SCM_UNBOUND))))
      (else
       (Scm_Error "zoom must be <real> or (<real> <real>), but bot %S" zoom)))
    (set! (-> gsprite zoom-x) zoom-x
          (-> gsprite zoom-y) zoom-y)))

(define-cproc sprite-zoom (gsprite::<graviton-sprite>)
  (let* ((zoom-x::double (-> gsprite zoom-x))
         (zoom-y::double (-> gsprite zoom-y)))
    (cond
      ((== zoom-x zoom-y)
       (return (Scm_MakeFlonum zoom-x)))
      (else
       (return (SCM_LIST2 (Scm_MakeFlonum zoom-x) (Scm_MakeFlonum zoom-y)))))))

(define-cproc set-sprite-visible! (gsprite::<graviton-sprite> visible::<boolean>)
  ::<void>
  (set! (-> gsprite visible) visible))

(define-cproc sprite-visible? (gsprite::<graviton-sprite>)
  ::<boolean>
  (return (-> gsprite visible)))

(define-cproc set-sprite-color! (gsprite::<graviton-sprite> color::<uint32>)
  ::<void>
  (set! (-> gsprite color) color))

(define-cproc sprite-color (gsprite::<graviton-sprite>)
  ::<uint32>
  (return (-> gsprite color)))

(define-cproc set-sprite-clip! (gsprite::<graviton-sprite> x::<int> y::<int> w::<int> h::<int>)
  ::<void>
  (let* ((clip::SDL_Rect* (SCM_NEW SDL_Rect)))
    (set! (-> clip x) x
          (-> clip y) y
          (-> clip w) w
          (-> clip h) h
          (-> gsprite clip) clip)))

(define-cproc sprite-clip-x (gsprite::<graviton-sprite>)
  ::<top>
  (let* ((clip::SDL_Rect* (-> gsprite clip)))
    (cond
      (clip
       (return (SCM_MAKE_INT (-> clip x))))
      (else
       (return SCM_FALSE)))))

(define-cproc sprite-clip-y (gsprite::<graviton-sprite>)
  ::<top>
  (let* ((clip::SDL_Rect* (-> gsprite clip)))
    (cond
      (clip
       (return (SCM_MAKE_INT (-> clip y))))
      (else
       (return SCM_FALSE)))))

(define-cproc sprite-clip-width (gsprite::<graviton-sprite>)
  ::<top>
  (let* ((clip::SDL_Rect* (-> gsprite clip)))
    (cond
      (clip
       (return (SCM_MAKE_INT (-> clip w))))
      (else
       (return SCM_FALSE)))))

(define-cproc sprite-clip-height (gsprite::<graviton-sprite>)
  ::<top>
  (let* ((clip::SDL_Rect* (-> gsprite clip)))
    (cond
      (clip
       (return (SCM_MAKE_INT (-> clip h))))
      (else
       (return SCM_FALSE)))))

(define (sprite-clip sprite)
  (values (sprite-clip-x sprite)
          (sprite-clip-y sprite)
          (sprite-clip-width sprite)
          (sprite-clip-height sprite)))
