;;;
;;; image.scm - Image & Texture
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
            "SDL_image.h"
            "gauche.h"
            "gauche/extend.h"
            "graviton.h"
            "stdbool.h"
            ))

 (define-cfn convert-coordinate (param::GrvTransformParam* x::double y::double ox::int* oy::int*)
   ::void
   (let* ((m00::double (-> param m00))
          (m10::double (-> param m10))
          (m01::double (-> param m01))
          (m11::double (-> param m11))
          (x0::double (-> param x0))
          (y0::double (-> param y0)))
     (set! (* ox) (cast int (round (+ (* m00 x) (* m01 y) x0)))
           (* oy) (cast int (round (+ (* m10 x) (* m11 y) y0))))))

 (define-cfn compute-transform-param (param::GrvTransformParam*
                                      width::int
                                      height::int
                                      x0::double
                                      y0::double
                                      x1::double
                                      y1::double
                                      clip?::bool)
   ::void
   (let* ((wex::int (- width 1))
          (wey::int (- height 1))
          (wcx::double (/ wex 2.0))
          (wcy::double (/ wey 2.0))
          (rx::double (/ (- x1 x0) wex))
          (ry::double (/ (- y1 y0) wey))
          (r::double (?: (< (fabs rx) (fabs ry)) (fabs ry) (fabs rx)))
          (mx::double (?: (< rx 0.0) (- r) r))
          (my::double (?: (< ry 0.0) (- r) r))
          (cx::double (/ (+ x0 x1) 2.0))
          (cy::double (/ (+ y0 y1) 2.0))
          (sx::double (+ (* mx (- wcx)) cx))
          (sy::double (+ (* my (- wcy)) cy))
          (ex::double (+ (* mx wcx) cx))
          (ey::double (+ (* my wcy) cy)))
     (set! (-> param m00) (/ 1.0 mx)
           (-> param m01) 0.0
           (-> param m10) 0.0
           (-> param m11) (/ 1.0 my)
           (-> param x0) (- (/ sx mx))
           (-> param y0) (- (/ sy my))
           (-> param left) (?: clip? x0 sx)
           (-> param top) (?: clip? y0 sy)
           (-> param right) (?: clip? x1 ex)
           (-> param bottom) (?: clip? y1 ey))))

 (define-cfn get-transform-param (gimage::GrvImage*)
   ::GrvTransformParam*
   (return (& (-> gimage param))))

 (define-cfn finalize-image (z data::void*)
   ::void
   (when (GRV_IMAGEP z)
     (let* ((gimage::GrvImage* (GRV_IMAGE_PTR z)))
       (unless (SCM_NULLP (-> gimage texture-alist))
         (fprintf stderr "[BUG] texture_alist in <graviton-image> must be nil in finalizer. forgot to call Grv_ReleaseTexture?\n"))
       (SDL_FreeSurface (-> gimage surface))
       (set! (-> gimage surface) NULL
             (-> gimage texture-alist) SCM_NIL))))

 (define-cfn Grv_SetNeedsRefreshImage (gimage::GrvImage* x::int y::int w::int h::int)
   ::void
   (when (SCM_NULLP (-> gimage texture-alist))
     (return))

   (let* ((rect::SDL_Rect* (& (-> gimage update_rect))))
     (cond
       ((SDL_RectEmpty rect)
        (set! (-> rect x) x
              (-> rect y) y
              (-> rect w) w
              (-> rect h) h))
       (else
        (let* ((rect-a::SDL_Rect)
               (rect-b::SDL_Rect)
               (rect-c::SDL_Rect)
               (rect-x::SDL_Rect))
          (set! (ref rect-a x) (-> rect x)
                (ref rect-a y) (-> rect y)
                (ref rect-a w) (-> rect w)
                (ref rect-a h) (-> rect h)
                (ref rect-b x) x
                (ref rect-b y) y
                (ref rect-b w) w
                (ref rect-b h) h
                (ref rect-c x) 0
                (ref rect-c y) 0
                (ref rect-c w) (-> gimage surface w)
                (ref rect-c h) (-> gimage surface h))
          (SDL_UnionRect (& rect-a) (& rect-b) (& rect-x))
          (SDL_IntersectRect (& rect-c) (& rect-x) (& (-> gimage update_rect))))))))

 (define-cfn create-streaming-texture-from-surface (renderer::SDL_Renderer* surface::SDL_Surface*)
   ::SDL_Texture*
   (let* ((w::int (-> surface w))
          (h::int (-> surface h))
          (rect::SDL_Rect)
          (texture::SDL_Texture* (SDL_CreateTexture renderer
                                                    (-> surface format format)
                                                    SDL_TEXTUREACCESS_STREAMING
                                                    w
                                                    h))
          (pixels::void*)
          (pitch::int))
     (when (== texture NULL)
       (Scm_Error "SDL_CreateTexture failed: %s" (SDL_GetError)))
     (set! (ref rect x) 0
           (ref rect y) 0
           (ref rect w) w
           (ref rect h) h)
     (when (< (SDL_LockSurface surface) 0)
       (Scm_Error "SDL_LockSurface failed: %s" (SDL_GetError)))
     (when (< (SDL_LockTexture texture (& rect) (& pixels) (& pitch)) 0)
       (Scm_Error "SDL_LockTexture failed: %s" (SDL_GetError)))
     (memcpy pixels (-> surface pixels) (* h pitch))
     (SDL_UnlockTexture texture)
     (SDL_UnlockSurface surface)
     (SDL_SetTextureBlendMode texture SDL_BLENDMODE_BLEND)
     (return texture)))

 (define-cfn update-texture (texture::SDL_Texture* surface::SDL_Surface* rect::SDL_Rect*)
   ::void
   (let* ((pixels::void*)
          (pitch::int)
          (y::int))
     (when (< (SDL_LockSurface surface) 0)
       (Scm_Error "SDL_LockSurface failed: %s" (SDL_GetError)))
     (when (< (SDL_LockTexture texture rect (& pixels) (& pitch)) 0)
       (Scm_Error "SDL_LockTexture failed: %s" (SDL_GetError)))
     (for ((set! y (-> rect y)) (< y (+ (-> rect y) (-> rect h))) (pre++ y))
       (memcpy (+ pixels (* (- y (-> rect y)) pitch))
               (+ (-> surface pixels) (* y (-> surface pitch)) (* (-> rect x) (-> surface format BytesPerPixel)))
               pitch))
     (SDL_UnlockTexture texture)
     (SDL_UnlockSurface surface)))

 (define-cfn Grv_RetainTexture (win gimage::GrvImage*)
   ::void
   (unless (GRV_WINDOWP win)
     (Scm_Error "<graviton-window> required, but got %S" win))

   (let* ((texture SCM_FALSE))
     (for-each (lambda (pair)
                 (when (SCM_EQ (SCM_CAR pair) win)
                   (set! texture (SCM_CDR pair))
                   (break)))
               (-> gimage texture-alist))
     (cond
       ((SCM_FALSEP texture)
        (let* ((renderer::SDL_Renderer* (-> (GRV_WINDOW_PTR win) renderer))
               (gtexture::GrvTexture* (SCM_NEW (.type GrvTexture)))
               (stexture::SDL_Texture* (create-streaming-texture-from-surface renderer (-> gimage surface))))
          (when (== stexture NULL)
            (Scm_Error "SDL_CreateTextureFromSurface failed: %s" (SDL_GetError)))
          (SDL_SetTextureBlendMode stexture SDL_BLENDMODE_BLEND)
          (set! (-> gtexture texture) stexture
                (-> gtexture ref_count) 1
                texture (GRV_TEXTURE_BOX gtexture)
                (-> gimage texture-alist) (Scm_Cons (Scm_Cons win texture) (-> gimage texture-alist) ))))
       (else
        (let* ((gtexture::GrvTexture* (GRV_TEXTURE_PTR texture)))
          (pre++ (-> gtexture ref_count)))))))

 (define-cfn Grv_ReleaseTexture (win gimage::GrvImage*)
   ::void
   (unless (GRV_WINDOWP win)
     (Scm_Error "<graviton-window> required, but got %S" win))

   (let* ((texture SCM_FALSE))
     (for-each (lambda (pair)
                 (when (SCM_EQ (SCM_CAR pair) win)
                   (set! texture (SCM_CDR pair))
                   (break)))
               (-> gimage texture-alist))
     (unless (SCM_FALSEP texture)
       (let* ((gtexture::GrvTexture* (GRV_TEXTURE_PTR texture)))
         (pre-- (-> gtexture ref_count))
         (when (<= (-> gtexture ref_count) 0)
           (unless (== (-> gtexture texture) NULL)
             (SDL_DestroyTexture (-> gtexture texture))
             (set! (-> gtexture texture) NULL))
           (let* ((new-alist SCM_NIL))
             (for-each (lambda (pair)
                         (unless (SCM_EQ (SCM_CAR pair) win)
                           (set! new-alist (Scm_Cons pair new-alist))))
                       (-> gimage texture-alist))
             (set! (-> gimage texture-alist) new-alist))))))

   (when (SCM_NULLP (-> gimage texture-alist))
     (set! (ref (-> gimage update_rect) x) 0
           (ref (-> gimage update_rect) y) 0
           (ref (-> gimage update_rect) w) 0
           (ref (-> gimage update_rect) h) 0)))

 (define-cfn Grv_RefreshTextures (gimage::GrvImage*)
   ::void
   (when (SDL_RectEmpty (& (-> gimage update_rect)))
     (return))

   (for-each (lambda (pair)
               (let* ((gtexture::GrvTexture* (GRV_TEXTURE_PTR (SCM_CDR pair)))
                      (format::Uint32)
                      (access::int)
                      (w::int)
                      (h::int))
                 (when (< (SDL_QueryTexture (-> gtexture texture) (& format) (& access) (& w) (& h)) 0)
                   (Scm_Error "SDL_QueryTexture failed: %s" (SDL_GetError)))
                 (cond
                   ((!= access SDL_TEXTUREACCESS_STREAMING)
                    (SDL_DestroyTexture (-> gtexture texture))
                    (let* ((renderer::SDL_Renderer* (-> (GRV_WINDOW_PTR (SCM_CAR pair)) renderer)))
                      (set! (-> gtexture texture) (create-streaming-texture-from-surface renderer (-> gimage surface)))))
                   (else
                    (update-texture (-> gtexture texture) (-> gimage surface) (& (-> gimage update_rect)))))))
             (-> gimage texture-alist))
   (set! (ref (-> gimage update_rect) x) 0
         (ref (-> gimage update_rect) y) 0
         (ref (-> gimage update_rect) w) 0
         (ref (-> gimage update_rect) h) 0))

 (define-cfn Grv_RetrieveTexture (win gimage::GrvImage*)
   ::SDL_Texture*
   (let* ((texture SCM_FALSE))
     (for-each (lambda (pair)
                 (when (SCM_EQ (SCM_CAR pair) win)
                   (set! texture (SCM_CDR pair))
                   (break)))
               (-> gimage texture-alist))
     (unless (GRV_TEXTUREP texture)
       (Scm_Error "<graviton-texture> not found for %S (got %S), forgot Grv_RetainTexture?" win texture))

     (return (-> (GRV_TEXTURE_PTR texture) texture))))

 (define-cfn image-pixel-width (gimage::GrvImage*)
   ::double
   (return (/ 1.0 (fabs (ref (-> gimage param) m00)))))

 (define-cfn image-pixel-height (gimage::GrvImage*)
   ::double
   (return (/ 1.0 (fabs (ref (-> gimage param) m11)))))

 (define-cfn Grv_ComputeImageCoordinate (gimage::GrvImage* x::double y::double ox::int* oy::int*)
   ::void
   (convert-coordinate (& (-> gimage param)) x y ox oy))

 (define-cfn Grv_Bitblt (src-gimage::GrvImage* src-rect::SDL_Rect* dst-gimage::GrvImage* dst-rect::SDL_Rect* color::Uint32)
   ::void
   (let* ((r::Uint8)
          (g::Uint8)
          (b::Uint8)
          (a::Uint8))
     (Grv_DecomposeRGBA color (& r) (& g) (& b) (& a))
     (when (< (SDL_SetSurfaceAlphaMod (-> src-gimage surface) a) 0)
       (Scm_Error "SDL_SetSurfaceAlphaMod failed: %s" (SDL_GetError)))
     (when (< (SDL_SetSurfaceColorMod (-> src-gimage surface) r g b) 0)
       (Scm_Error "SDL_SetSurfaceColorMod failed: %s" (SDL_GetError)))
     (when (< (SDL_SetSurfaceBlendMode (-> src-gimage surface) SDL_BLENDMODE_BLEND) 0)
       (Scm_Error "SDL_SetSurfaceBlendMode failed: %s" (SDL_GetError)))
     (cond
       ((and (== (-> src-rect w) (-> dst-rect w))
             (== (-> src-rect h) (-> dst-rect h)))
        (when (< (SDL_BlitSurface (-> src-gimage surface) src-rect (-> dst-gimage surface) dst-rect) 0)
          (Scm_Error "SDL_BlitSurface failed: %s" (SDL_GetError))))
       (else
        (when (< (SDL_BlitScaled (-> src-gimage surface) src-rect (-> dst-gimage surface) dst-rect) 0)
          (Scm_Error "SDL_BlitScaled failed: %s" (SDL_GetError)))))
     (Grv_SetNeedsRefreshImage dst-gimage (-> dst-rect x) (-> dst-rect y) (-> dst-rect w) (-> dst-rect h))))
 ) ;; end of inline-stub

(include "types.scm")

(define-cproc make-image (w::<int> h::<int>)
  (let* ((surface::SDL_Surface* (SDL_CreateRGBSurfaceWithFormat 0 w h 32 SDL_PIXELFORMAT_RGBA32))
         (gimage::GrvImage* (SCM_NEW (.type GrvImage)))
         (obj (GRV_IMAGE_BOX gimage)))
    (when (== surface NULL)
      (Scm_Error "SDL_CreateRGBSurfaceWithFormat failed: %s" (SDL_GetError)))
    (SDL_SetSurfaceBlendMode surface SDL_BLENDMODE_BLEND)
    (set! (-> gimage surface) surface
          (-> gimage texture-alist) SCM_NIL
          (ref (-> gimage update_rect) x) 0
          (ref (-> gimage update_rect) y) 0
          (ref (-> gimage update_rect) w) 0
          (ref (-> gimage update_rect) h) 0)
    (compute-transform-param (& (-> gimage param)) w h 0 0 (- w 1) (- h 1) false)
    (Scm_RegisterFinalizer obj finalize-image NULL)
    (return obj)))

(define-cproc load-image (filename::<const-cstring>)
  ::<graviton-image>
  (let* ((img-surface::SDL_Surface* (IMG_Load filename)))
    (when (== img-surface NULL)
      (Scm_Error "IMG_Load failed: %s" (IMG_GetError)))
    (let* ((image::GrvImage* (SCM_NEW (.type GrvImage)))
           (surface::SDL_Surface* (SDL_ConvertSurfaceFormat img-surface SDL_PIXELFORMAT_RGBA32 0)))
      (when (== surface NULL)
        (Scm_Error "SDL_ConvertSurface failed: %s" (SDL_GetError)))
      (SDL_FreeSurface img-surface)
      (SDL_SetSurfaceBlendMode surface SDL_BLENDMODE_BLEND)
      (set! (-> image surface) surface
            (-> image texture-alist) SCM_NIL
            (ref (-> image update_rect) x) 0
            (ref (-> image update_rect) y) 0
            (ref (-> image update_rect) w) 0
            (ref (-> image update_rect) h) 0)
      (return image))))

(define-cproc set-image-border! (gimage::<graviton-image> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (compute-transform-param (& (-> gimage param)) (-> gimage surface w) (-> gimage surface h) left top right bottom false))

(define-cproc image-size (image)
  ::<list>
  (cond
    ((GRV_IMAGEP image)
     (let* ((gimage::GrvImage* (GRV_IMAGE_PTR image)))
       (return (SCM_LIST2 (SCM_MAKE_INT (-> gimage surface w)) (SCM_MAKE_INT (-> gimage surface h))))))
    ((GRV_TILE_IMAGEP image)
     (let* ((gtile::GrvTileImage* (GRV_TILE_IMAGE_PTR image)))
       (return (SCM_LIST2 (SCM_MAKE_INT (ref (-> gtile rect) w)) (SCM_MAKE_INT (ref (-> gtile rect) h))))))
    (else
     (Scm_Error "<graviton-image> or <graviton-tile-image> required, but got %S" image))))

(define-cproc set-image-rgba-pixels! (image::<graviton-image> pixels::<u32vector>)
  ::<void>
  (let* ((surface::SDL_Surface* (-> image surface))
         (len::int (* (-> surface w) (-> surface h) 4)))
    (SDL_LockSurface surface)
    (memcpy (-> surface pixels)
            (SCM_U32VECTOR_ELEMENTS pixels)
            (?: (< len (SCM_U32VECTOR_SIZE pixels)) len (SCM_U32VECTOR_SIZE pixels)))
    (SDL_UnlockSurface surface)
    (set! (ref (-> image update_rect) x) 0
          (ref (-> image update_rect) y) 0
          (ref (-> image update_rect) w) (-> surface w)
          (ref (-> image update_rect) h) (-> surface h))))

(define-cproc image-rgba (image::<graviton-image> x::<double> y::<double>)
  ::<int32>
  (let* ((ox::int)
         (oy::int))
    (Grv_ComputeImageCoordinate image x y (& ox) (& oy))
    (unless (and (<= 0 ox) (< ox (-> image surface w))
                 (<= 0 oy) (< oy (-> image surface h)))
      (Scm_Error "(%f, %f) is out of image" x y))
    (SDL_LockSurface (-> image surface))
    (let* ((i::int (+ (* oy (-> image surface pitch)) (* ox (-> image surface format BytesPerPixel))))
           (v::Uint32 (aref (cast Uint32* (-> image surface pixels)) i)))
      (SDL_UnlockSurface (-> image surface))
      (return v))))

(define-cproc image-rgba-pixels (image::<graviton-image>)
  (let* ((surface::SDL_Surface* (-> image surface))
         (len::int (* (-> surface w) (-> surface h) 4))
         (vec (Scm_MakeU32Vector len 0)))
    (SDL_LockSurface surface)
    (memcpy (SCM_U32VECTOR_ELEMENTS vec) (-> surface pixels) len)
    (SDL_UnlockSurface surface)
    (return vec)))

(define-cproc bitblt (src-image dst-gimage::<graviton-image> dst-position::<list>
                                :key (src-position #f) (src-size #f) (dst-size #f) (color::<uint32> #xffffffff))
  ::<void>
  (let* ((src-gimage::GrvImage*)
         (src-rect::SDL_Rect*)
         (dst-rect::SDL_Rect))
    (cond
      ((GRV_TILE_IMAGEP src-image)
       (unless (SCM_FALSEP src-position)
         (Scm_Error "src-position can't be specified when src-image is <graviton-tile-image>"))
       (unless (SCM_FALSEP src-size)
         (Scm_Error "src-size can't be specified when src-image is <graviton-tile-image>"))
       (set! src-gimage (GRV_IMAGE_PTR (-> (GRV_TILE_IMAGE_PTR src-image) image))
             src-rect (& (-> (GRV_TILE_IMAGE_PTR src-image) rect))))
      ((GRV_IMAGEP src-image)
       (set! src-gimage (GRV_IMAGE_PTR src-image)
             src-rect (SCM_NEW SDL_Rect))
       (cond
         ((and (SCM_FALSEP src-position) (SCM_FALSEP src-size))
          (set! (-> src-rect x) 0
                (-> src-rect y) 0
                (-> src-rect w) (-> src-gimage surface w)
                (-> src-rect h) (-> src-gimage surface h)))
         ((and (SCM_LISTP src-position)
               (SCM_REALP (Scm_ListRef src-position 0 SCM_UNBOUND))
               (SCM_REALP (Scm_ListRef src-position 1 SCM_UNBOUND))
               (SCM_LISTP src-size)
               (SCM_INTP (Scm_ListRef src-size 0 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef src-size 1 SCM_UNBOUND)))
          (Grv_ComputeImageCoordinate src-gimage
                                      (Scm_GetDouble (Scm_ListRef src-position 0 SCM_UNBOUND))
                                      (Scm_GetDouble (Scm_ListRef src-position 1 SCM_UNBOUND))
                                      (& (-> src-rect x))
                                      (& (-> src-rect y)))
          (set! (-> src-rect w) (SCM_INT_VALUE (Scm_ListRef src-size 0 SCM_UNBOUND))
                (-> src-rect h) (SCM_INT_VALUE (Scm_ListRef src-size 1 SCM_UNBOUND))))
         (else
          (Scm_Error "(<real> <real>) for src-position and (<integer> <integer>) for src-size required, but got %S and %S"
                     src-position
                     src-size))))
      (else
       (Scm_Error "<graviton-tile-image> or <graviton-image> required, but got %S" src-image)))
    (unless (and (SCM_REALP (Scm_ListRef dst-position 0 SCM_UNBOUND))
                 (SCM_REALP (Scm_ListRef dst-position 1 SCM_UNBOUND)))
      (Scm_Error "(<real> <real>) required, but got %S" dst-position))
    (Grv_ComputeImageCoordinate dst-gimage
                                (Scm_GetDouble (Scm_ListRef dst-position 0 SCM_UNBOUND))
                                (Scm_GetDouble (Scm_ListRef dst-position 1 SCM_UNBOUND))
                                (& (ref dst-rect x))
                                (& (ref dst-rect y)))
    (cond
      ((SCM_FALSEP dst-size)
       (set! (ref dst-rect w) (-> src-rect w)
             (ref dst-rect h) (-> src-rect h)))
      ((and (SCM_LISTP src-size)
            (SCM_INTP (Scm_ListRef src-size 0 SCM_UNBOUND))
            (SCM_INTP (Scm_ListRef src-size 1 SCM_UNBOUND)))
       (set! (ref dst-rect w) (SCM_INT_VALUE (Scm_ListRef dst-size 0 SCM_UNBOUND))
             (ref dst-rect h) (SCM_INT_VALUE (Scm_ListRef dst-size 1 SCM_UNBOUND))))
      (else
       (Scm_Error "(<integer> <integer>) required, but got %S" dst-size)))
    (Grv_Bitblt src-gimage src-rect dst-gimage (& dst-rect) color)))

(define-cproc pixel-size (gimage::<graviton-image>)
  ::(<double> <double>)
  (return (image-pixel-width gimage)
          (image-pixel-height gimage)))

(define-cproc border-left (gimage::<graviton-image>)
  ::<double>
  (return (ref (-> gimage param) left)))

(define-cproc border-top (gimage::<graviton-image>)
  ::<double>
  (return (ref (-> gimage param) top)))

(define-cproc border-right (gimage::<graviton-image>)
  ::<double>
  (return (ref (-> gimage param) right)))

(define-cproc border-bottom (gimage::<graviton-image>)
  ::<double>
  (return (ref (-> gimage param) bottom)))

(define-cproc image-center-point (gimage::<graviton-image>)
  ::<list>
  (let* ((param::GrvTransformParam* (& (-> gimage param))))
    (return (SCM_LIST2 (Scm_MakeFlonum (/ (+ (-> param left) (-> param right)) 2.0))
                       (Scm_MakeFlonum (/ (+ (-> param top) (-> param bottom)) 2.0))))))


;;;
;;; TileImage
;;;

(define-cproc make-tile-image (image x::<int> y::<int> w::<int> h::<int>)
  ::<graviton-tile-image>
  (let* ((gtile::GrvTileImage* (SCM_NEW (.type GrvTileImage))))
    (unless (GRV_IMAGEP image)
      (Scm_Error "<graviton-image> required, but got %S" image))
    (set! (-> gtile image) image
          (ref (-> gtile rect) x) x
          (ref (-> gtile rect) y) y
          (ref (-> gtile rect) w) w
          (ref (-> gtile rect) h) h)
    (return gtile)))
