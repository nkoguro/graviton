;;;
;;; graviton.scm - Graphics and sound module
;;;
;;;   Copyright (c) 2018 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton
  (use compile-stub)
  (use file.util)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.record)
  (use gauche.selector)
  (use gauche.threads)
  (use math.const)
  (use util.match)

  (export <graviton-window>
          <graviton-image>
          <graviton-sprite>

          <point>
          make-point
          point?
          point-x
          point-y

          make-window
          close-event-stream
          clear-window-sprites!
          destroy-all-windows
          send-close-window-event
          window-size
          set-window-size!
          window-maximized?
          maximize-window
          window-minimized?
          minimize-window
          window-position
          set-window-position!
          window-title
          set-window-title!
          window-resizable?
          set-window-resizable!
          show-window
          window-shown?
          hide-window
          window-hidden?
          raise-window
          restore-window
          window-focused?
          window-icon
          set-window-icon!
          window?
          focus-window
          blur-window

          display-new-window

          next-event
          event-for-each
          frame-per-second
          set-frame-per-second!
          start-global-event-loop

          display-image
          make-image
          load-image
          put-image
          image-rgba-pixels
          set-image-rgba-pixels!
          sprite-center-position
          set-sprite-center-position!
          image-size
          image-width
          image-height

          set-border!
          border-left
          border-top
          border-right
          border-bottom
          border-min-x
          border-max-x
          border-min-y
          border-max-y
          center-point
          center-x
          center-y

          pixel-size
          pixel-width
          pixel-height

          draw-point
          draw-rect
          draw-line
          draw-polygon
          draw-circle

          message-box

          call-on-main-thread

          graviton-read-eval-print-loop
          )

  (extend graviton.color))

(select-module graviton)

(define-record-type (<point> (pseudo-rtd <list>))
  make-point point?
  (x point-x)
  (y point-y))

(inline-stub
  (declcode
   (.include "SDL.h"
             "SDL_image.h"
             "SDL_mixer.h"
             "float.h"
             "gauche.h"
             "gauche/number.h"
             "gauche/vector.h"
             "stdbool.h"
             "stdio.h"
             "string.h")

   (define-ctype TransformParam::(.struct
                                  (m00::double
                                   m01::double
                                   m10::double
                                   m11::double
                                   x0::double
                                   y0::double
                                   left::double
                                   top::double
                                   right::double
                                   bottom::double)))

   (define-ctype GrvTexture::(.struct
                              (texture::SDL_Texture*
                               ref_count::int)))

   (define-ctype GrvImage::(.struct
                            (surface::SDL_Surface*
                             update_rect::SDL_Rect
                             param::TransformParam
                             texture_alist)))

   (define-ctype GrvSprite::(.struct
                             (window
                              image
                              srcrect::SDL_Rect
                              center_x::double
                              center_y::double
                              z::double
                              angle::double
                              zoom_x::double
                              zoom_y::double
                              flip::SDL_RendererFlip
                              visible::bool)))

   (define-ctype GrvWindow::(.struct
                             (window::SDL_Window*
                              renderer::SDL_Renderer*
                              proc
                              events
                              sprites
                              icon
                              param::TransformParam)))

   (define-ctype ScratchArea::(.struct
                               (x::int
                                y::int
                                w::int
                                h::int
                                data::char*)))

   (define-ctype EventLoopStatus::(.struct
                                   (lock::SDL_SpinLock
                                    running?::bool)))

   (define-ctype ProcPacket::(.struct
                              (proc
                               lock::SDL_mutex*
                               cond::SDL_cond*
                               eval-packet::ScmEvalPacket*)))

   (define-ctype ProcQueue::(.struct
                             (buf::ProcPacket**
                              size::int
                              start::int
                              tail::int)))
   ) ;; end of declcode

  (define-cvar main-thread-id::SDL_threadID :static)
  (define-cvar grv-windows :static SCM_NIL)
  (define-cvar event-loop-status::EventLoopStatus)
  (define-cvar graviton-event::Uint32 :static)
  (define-cvar frame-per-second::int :static 30)
  (define-cvar event-loop-ticks::Uint32 :static 33)
  (define-cvar proc-queue::ProcQueue :static)

  (.define GRV_CALL_EVENT_CODE 1)
  (.define INITIAL_PROC_QUEUE_LENGTH 1000)

  (define-cptr <graviton-window> :private
    "GrvWindow*" "GravitonWindowClass" "GRV_WINDOW_P" "MAKE_GRV_WINDOW" "GRV_WINDOW_PTR")

  (define-cptr <graviton-image> :private
    "GrvImage*" "GravitonImageClass" "GRV_IMAGE_P" "MAKE_GRV_IMAGE" "GRV_IMAGE_PTR")

  (define-cptr <graviton-texture> :private
    "GrvTexture*" "GravitonTextureClass" "GRV_TEXTURE_P" "MAKE_GRV_TEXTURE" "GRV_TEXTURE_PTR")

  (define-cptr <graviton-sprite> :private
    "GrvSprite*" "GravitonSpriteClass" "GRV_SPRITE_P" "MAKE_GRV_SPRITE" "GRV_SPRITE_PTR")
  )  ;; end of inline-stub

(inline-stub
  (define-cfn teardown-libs (data::|void*|)
    ::void
    (Mix_CloseAudio)
    (Mix_Quit)

    (SDL_Quit))

  (define-cfn initialize-libs ()
    ::void
    (SDL_Init (logior SDL_INIT_VIDEO SDL_INIT_AUDIO))
    (Mix_Init (logior MIX_INIT_FLAC MIX_INIT_MOD MIX_INIT_MP3 MIX_INIT_OGG))
    (when (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 1024)
      (Scm_Error "Mix_OpenAudio failed: %s" (Mix_GetError)))
    (IMG_Init (logior IMG_INIT_JPG IMG_INIT_PNG IMG_INIT_TIF))

    (Scm_AddCleanupHandler teardown-libs NULL)

    (let* ((event-type::Uint32 (SDL_RegisterEvents 1)))
      (when (== event-type #xffffffff)
        (Scm_Error "SDL_RegisterEvents failed: %s" (SDL_GetError)))
      (set! graviton-event event-type))

    (set! main-thread-id (SDL_ThreadID))
    (set! (ref event-loop-status lock) 0
          (ref event-loop-status running?) false)
    (set! (ref proc-queue buf) (SCM_NEW_ARRAY (.type ProcPacket*) INITIAL_PROC_QUEUE_LENGTH)
          (ref proc-queue size) INITIAL_PROC_QUEUE_LENGTH
          (ref proc-queue start) 0
          (ref proc-queue tail) 0)
    ;; (SDL_LogSetPriority SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_DEBUG)
    )

  (initcode
   (initialize-libs))
  ) ;; end of inline-stub

;;;
;;; Coordinate calculation
;;;

(inline-stub
  (define-cfn init-transform-param (param::TransformParam* width::int height::int)
    ::void
    (set! (-> param m00) 1.0
          (-> param m01) 0.0
          (-> param m10) 0.0
          (-> param m11) 1.0
          (-> param x0) 0.0
          (-> param y0) 0.0
          (-> param left) 0.0
          (-> param top) 0.0
          (-> param right) (cast double (- width 1))
          (-> param bottom) (cast double (- height 1))))

  (define-cfn convert-coordinate (param::TransformParam* x::double y::double ox::int* oy::int*)
    ::void
    (let* ((m00::double (-> param m00))
           (m10::double (-> param m10))
           (m01::double (-> param m01))
           (m11::double (-> param m11))
           (x0::double (-> param x0))
           (y0::double (-> param y0)))
      (set! (* ox) (+ (* m00 x) (* m01 y) x0)
            (* oy) (+ (* m10 x) (* m11 y) y0))))

  (define-cfn compute-transform-param (param::TransformParam*
                                       width::int
                                       height::int
                                       x0::double
                                       y0::double
                                       x1::double
                                       y1::double)
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
            (-> param left) sx
            (-> param top) sy
            (-> param right) ex
            (-> param bottom) ey)))

  (define-cfn get-transform-param (window-or-image)
    ::TransformParam*
    (cond
      ((GRV_WINDOW_P window-or-image)
       (return (& (-> (GRV_WINDOW_PTR window-or-image) param))))
      ((GRV_IMAGE_P window-or-image)
       (return (& (-> (GRV_IMAGE_PTR window-or-image) param))))
      (else
       (Scm_Error "<graviton-window> or <graviton-image> required, but got %S" window-or-image))))
  ) ;; end of inline-stub

;;;
;;; Image and Texture
;;;

(inline-stub
  (define-cfn finalize-image (z data::void*)
    ::void
    (when (GRV_IMAGE_P z)
      (let* ((gimage::GrvImage* (GRV_IMAGE_PTR z)))
        (unless (SCM_NULLP (-> gimage texture_alist))
          (fprintf stderr "[BUG] texture_alist in <graviton-image> must be nil in finalizer. forgot to call release-texture?\n"))
        (SDL_FreeSurface (-> gimage surface))
        (set! (-> gimage surface) NULL
              (-> gimage texture_alist) SCM_NIL))))

  (define-cfn update-rect (gimage::GrvImage* x::int y::int w::int h::int)
    ::void
    (when (SCM_NULLP (-> gimage texture_alist))
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

  (define-cfn retain-texture (win gimage::GrvImage*)
    ::void
    (unless (GRV_WINDOW_P win)
      (Scm_Error "<graviton-window> required, but got %S" win))

    (let* ((texture SCM_FALSE))
      (for-each (lambda (pair)
                  (when (SCM_EQ (SCM_CAR pair) win)
                    (set! texture (SCM_CDR pair))
                    (break)))
                (-> gimage texture_alist))
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
                 texture (MAKE_GRV_TEXTURE gtexture)
                 (-> gimage texture_alist) (Scm_Cons (Scm_Cons win texture) (-> gimage texture_alist) ))))
        (else
         (let* ((gtexture::GrvTexture* (GRV_TEXTURE_PTR texture)))
           (pre++ (-> gtexture ref_count)))))))

  (define-cfn release-texture (win gimage::GrvImage*)
    ::void
    (unless (GRV_WINDOW_P win)
      (Scm_Error "<graviton-window> required, but got %S" win))

    (let* ((texture SCM_FALSE))
      (for-each (lambda (pair)
                  (when (SCM_EQ (SCM_CAR pair) win)
                    (set! texture (SCM_CDR pair))
                    (break)))
                (-> gimage texture_alist))
      (unless (SCM_FALSEP texture)
        (let* ((gtexture::GrvTexture* (GRV_TEXTURE_PTR texture)))
          (pre-- (-> gtexture ref_count))
          (when (<= (-> gtexture ref_count) 0)
            (unless (== (-> gtexture texture) NULL)
              (SDL_DestroyTexture (-> gtexture texture))
              (set! (-> gtexture texture) NULL))
            (let* ((prev SCM_NIL))
              (pair-for-each (lambda (rest)
                               (let* ((pair (SCM_CAR rest)))
                                 (when (SCM_EQ (SCM_CAR pair) win)
                                   (cond
                                     ((SCM_NULLP prev)
                                      (set! (-> gimage texture_alist) (SCM_CDR rest))
                                      (break))
                                     (else
                                      (SCM_SET_CDR prev (SCM_CDR rest))
                                      (break))))
                                 (set! prev pair)))
                             (-> gimage texture_alist)))))))

    (when (SCM_NULLP (-> gimage texture_alist))
      (set! (ref (-> gimage update_rect) x) 0
            (ref (-> gimage update_rect) y) 0
            (ref (-> gimage update_rect) w) 0
            (ref (-> gimage update_rect) h) 0)))

  (define-cfn refresh-textures (gimage::GrvImage*)
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
              (-> gimage texture_alist))
    (set! (ref (-> gimage update_rect) x) 0
          (ref (-> gimage update_rect) y) 0
          (ref (-> gimage update_rect) w) 0
          (ref (-> gimage update_rect) h) 0))

  (define-cfn get-texture (win gimage::GrvImage*)
    ::SDL_Texture*
    (let* ((texture SCM_FALSE))
      (for-each (lambda (pair)
                  (when (SCM_EQ (SCM_CAR pair) win)
                    (set! texture (SCM_CDR pair))
                    (break)))
                (-> gimage texture_alist))
      (when (SCM_FALSEP texture)
        (Scm_Error "<graviton-texture> not found for %S, forgot retain-texture?" win))

      (return (-> (GRV_TEXTURE_PTR texture) texture))))

  (define-cfn image-pixel-width (gimage::GrvImage*)
    ::double
    (return (/ 1.0 (fabs (ref (-> gimage param) m00)))))

  (define-cfn image-pixel-height (gimage::GrvImage*)
    ::double
    (return (/ 1.0 (fabs (ref (-> gimage param) m11)))))

  (define-cfn image-coordinate (gimage::GrvImage* x::double y::double ox::int* oy::int*)
    ::void
    (convert-coordinate (& (-> gimage param)) x y ox oy))
  ) ;; end of inline-stub

(define-cproc make-image (w::<int> h::<int>)
  (let* ((surface::SDL_Surface* (SDL_CreateRGBSurfaceWithFormat 0 w h 32 SDL_PIXELFORMAT_RGBA32))
         (gimage::GrvImage* (SCM_NEW (.type GrvImage)))
         (obj (MAKE_GRV_IMAGE gimage)))
    (when (== surface NULL)
      (Scm_Error "SDL_CreateRGBSurfaceWithFormat failed: %s" (SDL_GetError)))
    (SDL_SetSurfaceBlendMode surface SDL_BLENDMODE_BLEND)
    (set! (-> gimage surface) surface
          (-> gimage texture_alist) SCM_NIL
          (ref (-> gimage update_rect) x) 0
          (ref (-> gimage update_rect) y) 0
          (ref (-> gimage update_rect) w) 0
          (ref (-> gimage update_rect) h) 0)
    (init-transform-param (& (-> gimage param)) w h)
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
            (-> image texture_alist) SCM_NIL
            (ref (-> image update_rect) x) 0
            (ref (-> image update_rect) y) 0
            (ref (-> image update_rect) w) 0
            (ref (-> image update_rect) h) 0)
      (return image))))

(define-cproc set-image-border! (gimage::<graviton-image> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (compute-transform-param (& (-> gimage param)) (-> gimage surface w) (-> gimage surface h) left top right bottom))

(define-cproc image-size (image::<graviton-image>)
  ::<list>
  (return (SCM_LIST2 (SCM_MAKE_INT (-> image surface w)) (SCM_MAKE_INT (-> image surface h)))))

(define (image-width image)
  (list-ref (image-size image) 0))

(define (image-height image)
  (list-ref (image-size image) 1))

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

(define-cproc image-rgba (image::<graviton-image> x::<int> y::<int>)
  ::<int32>
  (unless (and (<= 0 x) (< x (-> image surface w))
               (<= 0 y) (< y (-> image surface h)))
    (Scm_Error "(x, y) must be in (0, 0)-(%d, %d), but got (%d, %d)" (-> image surface w) (-> image surface h) x y))
  (SDL_LockSurface (-> image surface))
  (let* ((i::int (+ (* y (-> image surface pitch)) (* x (-> image surface format BytesPerPixel))))
         (v::Uint32 (aref (cast Uint32* (-> image surface pixels)) i)))
    (SDL_UnlockSurface (-> image surface))
    (return v)))

(define-cproc image-rgba-pixels (image::<graviton-image>)
  (let* ((surface::SDL_Surface* (-> image surface))
         (len::int (* (-> surface w) (-> surface h) 4))
         (vec (Scm_MakeU32Vector len 0)))
    (SDL_LockSurface surface)
    (memcpy (SCM_U32VECTOR_ELEMENTS vec) (-> surface pixels) len)
    (SDL_UnlockSurface surface)
    (return vec)))

(define (image? obj)
  (is-a? obj <graviton-image>))


;;;
;;; Sprite
;;;

(inline-stub
  (define-cfn finalize-sprite (z data::void*)
    ::void
    (when (GRV_SPRITE_P z)
      (set! (-> (GRV_SPRITE_PTR z) window) SCM_FALSE
            (-> (GRV_SPRITE_PTR z) image) SCM_FALSE)))

  (define-cfn invalidate-sprite (gsprite::GrvSprite*)
    ::void
    (unless (SCM_FALSEP (-> gsprite image))
      (release-texture (-> gsprite window) (GRV_IMAGE_PTR (-> gsprite image))))
    (set! (-> gsprite window) SCM_FALSE))

  (define-cfn render-sprite (gsprite::GrvSprite*)
    ::void
    (when (or (SCM_FALSEP (-> gsprite image))
              (not (-> gsprite visible)))
      (return))
    (let* ((gimage::GrvImage* (GRV_IMAGE_PTR (-> gsprite image))))
      (refresh-textures gimage)
      (let* ((win (-> gsprite window))
             (gwin::GrvWindow* (GRV_WINDOW_PTR win))
             (texture::SDL_Texture* (get-texture win gimage))
             (spr-center-x::int)
             (spr-center-y::int)
             (spr-w::double (* (ref (-> gsprite srcrect) w) (-> gsprite zoom_x)))
             (spr-h::double (* (ref (-> gsprite srcrect) h) (-> gsprite zoom_y)))
             (dstrect::SDL_Rect))
        (window-coordinate gwin (-> gsprite center_x) (-> gsprite center_y) (& spr-center-x) (& spr-center-y))
        (set! (ref dstrect x) (cast int (round (- spr-center-x (/ spr-w 2.0))))
              (ref dstrect y) (cast int (round (- spr-center-y (/ spr-h 2.0))))
              (ref dstrect w) (cast int (round spr-w))
              (ref dstrect h) (cast int (round spr-h)))
        (SDL_RenderCopyEx (-> gwin renderer)
                          texture
                          (& (-> gsprite srcrect))
                          (& dstrect)
                          (-> gsprite angle)
                          NULL
                          (-> gsprite flip)))))

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
  ) ;; end of inline-stub

(define-cproc make-sprite (window
                           image
                           center-x::<double>
                           center-y::<double>
                           z::<double>
                           rect
                           angle::<double>
                           zoom_x::<double>
                           zoom_y::<double>
                           visible::<boolean>)
  (unless (GRV_WINDOW_P window)
    (Scm_Error "window must be <graviton-window>, but got %S" window))
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image))
    (Scm_Error "image must be <graviton-image> or #f, but got %S" image))
  (unless (and (SCM_LISTP rect)
               (== (Scm_Length rect) 4)
               (SCM_INTP (Scm_ListRef rect 0 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 1 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 2 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 3 SCM_UNBOUND)))
    (Scm_Error "rect must be (x y w h), but got %S" rect))
  (let* ((sprite::GrvSprite* (SCM_NEW (.type GrvSprite)))
         (flip::SDL_RendererFlip))
    (cond
      ((and (< zoom_x 0) (< zoom_y 0))
       (set! zoom_x (- zoom_x)
             zoom_y (- zoom_y)
             angle (+ angle 180)
             flip SDL_FLIP_NONE))
      ((< zoom_x 0)
       (set! zoom_x (- zoom_x)
             flip SDL_FLIP_VERTICAL))
      ((< zoom_y 0)
       (set! zoom_y (- zoom_y)
             flip SDL_FLIP_HORIZONTAL))
      (else
       (set! flip SDL_FLIP_NONE)))
    (set! (-> sprite window) window
          (-> sprite image) image
          (-> sprite center_x) center-x
          (-> sprite center_y) center-y
          (-> sprite z) z
          (ref (-> sprite srcrect) x) (SCM_INT_VALUE (Scm_ListRef rect 0 (SCM_MAKE_INT 0)))
          (ref (-> sprite srcrect) y) (SCM_INT_VALUE (Scm_ListRef rect 1 (SCM_MAKE_INT 0)))
          (ref (-> sprite srcrect) w) (SCM_INT_VALUE (Scm_ListRef rect 2 (SCM_MAKE_INT 0)))
          (ref (-> sprite srcrect) h) (SCM_INT_VALUE (Scm_ListRef rect 3 (SCM_MAKE_INT 0)))
          (-> sprite angle) angle
          (-> sprite zoom_x) zoom_x
          (-> sprite zoom_y) zoom_y
          (-> sprite flip) flip
          (-> sprite visible) visible)
    (unless (SCM_FALSEP image)
      (retain-texture window (GRV_IMAGE_PTR image)))
    (let* ((sprite-obj (MAKE_GRV_SPRITE sprite)))
      (Scm_RegisterFinalizer sprite-obj finalize-sprite NULL)
      (insert-window-sprite sprite-obj)
      (return sprite-obj))))

(define-cproc %set-sprite-image! (sprite::<graviton-sprite> image rect)
  ::<void>
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image))
    (Scm_Error "image must be <graviton-image> or #f, but got %S" image))
  (unless (and (SCM_LISTP rect)
               (== (Scm_Length rect) 4)
               (SCM_INTP (Scm_ListRef rect 0 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 1 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 2 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 3 SCM_UNBOUND)))
    (Scm_Error "rect must be (x y w h), but got %S" rect))
  (unless (SCM_FALSEP (-> sprite image))
    (release-texture (-> sprite window) (GRV_IMAGE_PTR (-> sprite image))))
  (unless (SCM_FALSEP image)
    (retain-texture (-> sprite window) (GRV_IMAGE_PTR image)))
  (set! (-> sprite image) image
        (ref (-> sprite srcrect) x) (SCM_INT_VALUE (Scm_ListRef rect 0 (SCM_MAKE_INT 0)))
        (ref (-> sprite srcrect) y) (SCM_INT_VALUE (Scm_ListRef rect 1 (SCM_MAKE_INT 0)))
        (ref (-> sprite srcrect) w) (SCM_INT_VALUE (Scm_ListRef rect 2 (SCM_MAKE_INT 0)))
        (ref (-> sprite srcrect) h) (SCM_INT_VALUE (Scm_ListRef rect 3 (SCM_MAKE_INT 0)))))

(define-cproc sprite-image (sprite::<graviton-sprite>)
  ::(<top> <top>)
  (return (-> sprite image)
          (SCM_LIST4 (SCM_MAKE_INT (ref (-> sprite srcrect) x))
                     (SCM_MAKE_INT (ref (-> sprite srcrect) y))
                     (SCM_MAKE_INT (ref (-> sprite srcrect) w))
                     (SCM_MAKE_INT (ref (-> sprite srcrect) h)))))

(define-cproc set-sprite-center-position! (sprite::<graviton-sprite> x::<double> y::<double>)
  ::<void>
  (set! (-> sprite center_x) x
        (-> sprite center_y) y))

(define-cproc sprite-center-position (sprite::<graviton-sprite>)
  ::(<double> <double>)
  (return (-> sprite center_x) (-> sprite center_y)))

(define-cproc set-sprite-z! (sprite z::<double>)
  ::<void>
  (unless (GRV_SPRITE_P sprite)
    (Scm_Error "<graviton-sprite> required, but got %S" sprite))
  (remove-window-sprite sprite)
  (set! (-> (GRV_SPRITE_PTR sprite) z) z)
  (insert-window-sprite sprite))

(define-cproc sprite-z (sprite::<graviton-sprite>)
  ::<double>
  (return (-> sprite z)))

(define-cproc set-sprite-angle! (sprite::<graviton-sprite> angle::<double>)
  ::<void>
  (set! (-> sprite angle) angle))

(define-cproc sprite-angle (sprite::<graviton-sprite>)
  ::<double>
  (return (-> sprite angle)))

(define-cproc set-sprite-zoom! (sprite::<graviton-sprite> zoom)
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
    (cond
      ((and (< zoom-x 0) (< zoom-y 0))
       (set! (-> sprite angle) (+ (-> sprite angle) 180)
             (-> sprite zoom_x) (- zoom-x)
             (-> sprite zoom_y) (- zoom-y)
             (-> sprite flip) SDL_FLIP_NONE))
      ((< zoom-x 0)
       (set! (-> sprite zoom_x) (- zoom-x)
             (-> sprite zoom_y) zoom_y
             (-> sprite flip) SDL_FLIP_VERTICAL))
      ((< zoom-y 0)
       (set! (-> sprite zoom_x) zoom-x
             (-> sprite zoom_y) (- zoom-y)
             (-> sprite flip) SDL_FLIP_HORIZONTAL))
      (else
       (set! (-> sprite zoom_x) zoom-x
             (-> sprite zoom_y) zoom-y
             (-> sprite flip) SDL_FLIP_NONE)))))

(define-cproc sprite-zoom (sprite::<graviton-sprite>)
  ::(<double> <double>)
  (case (-> sprite flip)
    ((SDL_FLIP_VERTICAL)
     (return (- (-> sprite zoom_x)) (-> sprite zoom_y)))
    ((SDL_FLIP_HORIZONTAL)
     (return (-> sprite zoom_x) (- (-> sprite zoom_y))))
    (else
     (return (-> sprite zoom_x) (-> sprite zoom_y)))))

(define-cproc set-sprite-visible! (sprite::<graviton-sprite> visible::<boolean>)
  ::<void>
  (set! (-> sprite visible) visible))

(define-cproc sprite-visible? (sprite::<graviton-sprite>)
  ::<boolean>
  (return (-> sprite visible)))

(define (set-sprite-image! sprite image :optional (rect (receive (w h) (image-size image)
                                                          (list 0 0 w h))))
  (%set-sprite-image! sprite image rect))

(define (put-image window image center-x center-y :key rect (z 0) (angle 0) (zoom-x 1.0) (zoom-y 1.0) (visible #t))
  (let1 rect (if (undefined? rect)
                 (list 0 0 (image-width image) (image-height image))
                 rect)
    (make-sprite window image center-x center-y z rect angle zoom-x zoom-y visible)))


;;;
;;; Window
;;;

(inline-stub
  (define-cfn register-grv-window (gwin::GrvWindow*)
    ::ScmObj
    (let* ((obj (MAKE_GRV_WINDOW gwin)))
      (set! grv-windows (Scm_Cons obj grv-windows))
      (return obj)))

  (define-cfn unregister-grv-window (gwin::GrvWindow*)
    ::void
    (let* ((prev SCM_NIL))
      (pair-for-each (lambda (pair)
                       (let* ((curwin::GrvWindow* (GRV_WINDOW_PTR (SCM_CAR pair))))
                         (when (== gwin curwin)
                           (cond
                             ((SCM_NULLP prev)
                              (set! grv-windows (SCM_CDR pair))
                              (break))
                             (else
                              (SCM_SET_CDR prev (SCM_CDR pair))
                              (break))))))
                     grv-windows)))

  (define-cfn destroy-window (gwin::GrvWindow*)
    ::void
    (for-each (lambda (sprite)
                (invalidate-sprite (GRV_SPRITE_PTR sprite)))
              (-> gwin sprites))
    (set! (-> gwin sprites) SCM_NIL)

    (unregister-grv-window gwin)
    (SDL_DestroyRenderer (-> gwin renderer))
    (SDL_DestroyWindow (-> gwin window))
    (set! (-> gwin window) NULL
          (-> gwin renderer) NULL
          (-> gwin proc) SCM_FALSE
          (-> gwin events) SCM_NIL))

  (define-cfn window-pixel-width (gwin::GrvWindow*)
    ::double
    (return (/ 1.0 (fabs (ref (-> gwin param) m00)))))

  (define-cfn window-pixel-height (gwin::GrvWindow*)
    ::double
    (return (/ 1.0 (fabs (ref (-> gwin param) m11)))))

  (define-cfn window-coordinate (gwin::GrvWindow* x::double y::double ox::int* oy::int*)
    ::void :static
    (convert-coordinate (& (-> gwin param)) x y ox oy))

  (define-cfn update-window-contents ()
    ::void
    (for-each (lambda (win)
                (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win))
                       (renderer::SDL_Renderer* (-> gwin renderer)))
                  (SDL_SetRenderDrawBlendMode renderer SDL_BLENDMODE_BLEND)
                  (SDL_SetRenderDrawColor renderer 0 0 0 255)
                  (SDL_RenderClear renderer)
                  (for-each (lambda (sprite)
                              (render-sprite (GRV_SPRITE_PTR sprite)))
                            (-> gwin sprites))
                  (SDL_RenderPresent renderer)))
              grv-windows))
  ) ;; end of inline-stub

(define-cproc make-window (title::<const-cstring> size proc :key (resizable?::<boolean> #f) (icon #f) (shown?::<boolean> #t) (maximized?::<boolean> #f) (minimized?::<boolean> #f))
  (let* ((width::int 0)
         (height::int 0)
         (flags::Uint32 0))
    (cond
      ((and (SCM_LISTP size)
            (== (Scm_Length size) 2)
            (SCM_INTP (Scm_ListRef size 0 SCM_UNBOUND))
            (SCM_INTP (Scm_ListRef size 1 SCM_UNBOUND)))
       (set! width (SCM_INT_VALUE (Scm_ListRef size 0 (SCM_MAKE_INT -1)))
             height (SCM_INT_VALUE (Scm_ListRef size 1 (SCM_MAKE_INT -1)))))
      ((SCM_EQ size 'fullscreen)
       (set! flags SDL_WINDOW_FULLSCREEN_DESKTOP))
      (else
       (Scm_Error "size must be a list of two integer elements or 'fullscreen, but got %S" size)))
    (when resizable?
      (set! flags (logior flags SDL_WINDOW_RESIZABLE)))
    (when maximized?
      (set! flags (logior flags SDL_WINDOW_MAXIMIZED)))
    (when minimized?
      (set! flags (logior flags SDL_WINDOW_MINIMIZED)))
    (cond
      (shown?
       (set! flags (logior flags SDL_WINDOW_SHOWN)))
      (else
       (set! flags (logior flags SDL_WINDOW_HIDDEN))))
    (unless (or (SCM_FALSEP icon) (GRV_IMAGE_P icon))
      (Scm_Error "icon must be <graviton-image> or #f, but got %S" icon))

    (let* ((gwin::GrvWindow* (SCM_NEW (.type GrvWindow))))
      (set! (-> gwin window) NULL
            (-> gwin renderer) NULL
            (-> gwin proc) proc
            (-> gwin events) SCM_NIL
            (-> gwin sprites) SCM_NIL
            (-> gwin icon) icon)

      (set! (-> gwin window) (SDL_CreateWindow title
                                               SDL_WINDOWPOS_UNDEFINED
                                               SDL_WINDOWPOS_UNDEFINED
                                               width
                                               height
                                               flags))
      (unless (-> gwin window)
        (Scm_Error "SDL_CreateWindow failed: %s" (SDL_GetError)))

      (when (GRV_IMAGE_P icon)
        (SDL_SetWindowIcon (-> gwin window) (-> (GRV_IMAGE_PTR icon) surface)))

      (let* ((w::int)
             (h::int))
        (SDL_GetWindowSize (-> gwin window) (& w) (& h))
        (init-transform-param (& (-> gwin param)) w h))

      (set! (-> gwin renderer) (SDL_CreateRenderer (-> gwin window) -1 SDL_RENDERER_PRESENTVSYNC))
      (unless (-> gwin renderer)
        (Scm_Error "SDL_CreateRenderer failed: %s" (SDL_GetError)))

      (return (register-grv-window gwin)))))

(define-cproc set-window-border! (gwin::<graviton-window> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (let* ((width::int)
         (height::int))
    (SDL_GetWindowSize (-> gwin window) (& width) (& height))
    (compute-transform-param (& (-> gwin param)) width height left top right bottom)))

(define-cproc set-window-proc! (gwin::<graviton-window> proc)
  (set! (-> gwin proc) proc))

(define-cproc %next-event (gwin::<graviton-window>)
  (cond
    ((SCM_NULLP (-> gwin events))
     (return SCM_FALSE))
    ((SCM_FALSEP (-> gwin events))
     (return SCM_EOF))
    (else
     (let* ((event (SCM_CAR (-> gwin events))))
       (set! (-> gwin events) (SCM_CDR (-> gwin events)))
       (return event)))))

(define-cproc close-event-stream (gwin::<graviton-window>)
  (set! (-> gwin events) SCM_FALSE))

(define-cproc clear-window-sprites! (gwin::<graviton-window>)
  ::<void>
  (for-each (lambda (sprite)
              (let* ((gsprite::GrvSprite* (GRV_SPRITE_PTR gsprite)))
                (invalidate-sprite gsprite)))
            (-> gwin sprites))
  (set! (-> gwin sprites) SCM_NIL))

(define-cproc destroy-all-windows ()
  ::<void>
  (for-each (lambda (obj)
              (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj)))
                (destroy-window gwin)))
            grv-windows)
  (set! grv-windows SCM_NIL))

(define-cproc send-close-window-event (gwin::<graviton-window>)
  ::<void>
  (unless (-> gwin window)
    (return))
  (let* ((event::SDL_Event))
    (set! (ref event window type) SDL_WINDOWEVENT
          (ref event window windowID) (SDL_GetWindowID (-> gwin window))
          (ref event window event) SDL_WINDOWEVENT_CLOSE)
    (SDL_PushEvent (& event))))

(define-cproc window-size (gwin::<graviton-window>)
  ::<list>
  (let* ((w::int)
         (h::int))
    (SDL_GetWindowSize (-> gwin window) (& w) (& h))
    (return (SCM_LIST2 (SCM_MAKE_INT w) (SCM_MAKE_INT h)))))

(define (window-width window)
  (list-ref (window-size window) 0))

(define (window-height window)
  (list-ref (window-size window) 1))

(define-cproc set-window-size! (gwin::<graviton-window> size)
  ::<void>
  (cond
    ((SCM_EQ size 'fullscreen)
     (when (< (SDL_SetWindowFullscreen (-> gwin window) SDL_WINDOW_FULLSCREEN_DESKTOP) 0)
       (Scm_Error "SDL_SetWindowFullscreen failed: %s" (SDL_GetError))))
    ((and (SCM_LISTP size)
          (SCM_INTP (Scm_ListRef size 0 SCM_UNBOUND))
          (SCM_INTP (Scm_ListRef size 1 SCM_UNBOUND)))
     (SDL_SetWindowSize (-> gwin window)
                        (SCM_INT_VALUE (Scm_ListRef size 0 SCM_UNBOUND))
                        (SCM_INT_VALUE (Scm_ListRef size 1 SCM_UNBOUND))))
    (else
     (Scm_Error "(<integer> <integer>) or 'fullscreen required, but got %S" size))))

(define-cproc window-fullscreen? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags (logior SDL_WINDOW_FULLSCREEN_DESKTOP SDL_WINDOW_FULLSCREEN)))))

(define-cproc window-maximized? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags SDL_WINDOW_MAXIMIZED))))

(define-cproc maximize-window (gwin::<graviton-window>)
  ::<void>
  (SDL_MaximizeWindow (-> gwin window)))

(define (set-window-maximized! window maximized?)
  (if maximized?
      (maximize-window window)
      (restore-window window)))

(define-cproc window-minimized? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags SDL_WINDOW_MINIMIZED))))

(define-cproc minimize-window (gwin::<graviton-window>)
  ::<void>
  (SDL_MinimizeWindow (-> gwin window)))

(define (set-window-minimized! window minimized?)
  (if minimized?
      (minimize-window window)
      (restore-window window)))

(define-cproc window-position (gwin::<graviton-window>)
  ::<list>
  (let* ((w::int)
         (h::int))
    (SDL_GetWindowPosition (-> gwin window) (& w) (& h))
    (return (SCM_LIST2 (SCM_MAKE_INT w) (SCM_MAKE_INT h)))))

(define-cproc set-window-position! (gwin::<graviton-window> position)
  ::<void>
  (cond
    ((and (SCM_LISTP position)
          (SCM_REALP (Scm_ListRef position 0 SCM_UNBOUND))
          (SCM_REALP (Scm_ListRef position 1 SCM_UNBOUND)))
     (SDL_SetWindowPosition (-> gwin window)
                            (cast int (round (Scm_GetDouble (Scm_ListRef position 0 SCM_UNBOUND))))
                            (cast int (round (Scm_GetDouble (Scm_ListRef position 1 SCM_UNBOUND))))))))

(define-cproc window-title (gwin::<graviton-window>)
  (return (SCM_MAKE_STR_COPYING (SDL_GetWindowTitle (-> gwin window)))))

(define-cproc set-window-title! (gwin::<graviton-window> title::<const-cstring>)
  ::<void>
  (SDL_SetWindowTitle (-> gwin window) title))

(define-cproc window-resizable? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags SDL_WINDOW_RESIZABLE))))

(define-cproc set-window-resizable! (gwin::<graviton-window> resizable?::<boolean>)
  ::<void>
  (SDL_SetWindowResizable (-> gwin window) resizable?))

(define-cproc show-window (gwin::<graviton-window>)
  ::<void>
  (SDL_ShowWindow (-> gwin window)))

(define-cproc window-shown? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags SDL_WINDOW_SHOWN))))

(define-cproc hide-window (gwin::<graviton-window>)
  ::<void>
  (SDL_HideWindow (-> gwin window)))

(define-cproc window-hidden? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags SDL_WINDOW_HIDDEN))))

(define-cproc raise-window (gwin::<graviton-window>)
  ::<void>
  (SDL_RaiseWindow (-> gwin window)))

(define-cproc restore-window (gwin::<graviton-window>)
  ::<void>
  (SDL_RestoreWindow (-> gwin window)))

(define-cproc window-focused? (gwin::<graviton-window>)
  ::<boolean>
  (return (SDL_GetWindowGrab (-> gwin window))))

(define-cproc set-window-focus! (gwin::<graviton-window> focus?::<boolean>)
  ::<void>
  (SDL_SetWindowGrab (-> gwin window) focus?))

(define-cproc window-icon (gwin::<graviton-window>)
  (return (-> gwin icon)))

(define-cproc set-window-icon! (gwin::<graviton-window> icon)
  ::<void>
  (cond
    ((GRV_IMAGE_P icon)
     (SDL_SetWindowIcon (-> gwin window) (-> (GRV_IMAGE_PTR icon) surface))
     (set! (-> gwin icon) icon))
    (else
     (Scm_Error "<graviton-image> required, but got %S" icon))))

(define (window? obj)
  (is-a? obj <graviton-window>))

(define (focus-window window)
  (set-window-focus! window #t))

(define (blur-window window)
  (set-window-focus! window #f))

;;;
;;; Event
;;;

(inline-stub
  (define-cfn enqueue-proc! (proc-packet::ProcPacket*)
    ::void
    (let* ((buf::ProcPacket** (ref proc-queue buf))
           (len::int (ref proc-queue size)))
      (set! (aref buf (ref proc-queue tail)) proc-packet)
      (set! (ref proc-queue tail) (% (+ (ref proc-queue tail) 1) len))
      (when (== (ref proc-queue start) (ref proc-queue tail))
        (let* ((i::int (ref proc-queue start))
               (j::int 0)
               (new-buf::ProcPacket** (SCM_NEW_ARRAY (.type ProcPacket*) (* len 2))))
          (while (< j len)
            (set! (aref new-buf j) (aref buf i)
                  i (% (+ i 1) len)
                  j (+ j 1)))
          (set! (ref proc-queue buf) new-buf
                (ref proc-queue size) (* len 2)
                (ref proc-queue start) 0
                (ref proc-queue tail) len)))))

  (define-cfn dequeue-proc! ()
    ::ProcPacket*
    (cond
      ((== (ref proc-queue start) (ref proc-queue tail))
       (return NULL))
      (else
       (let* ((buf::ProcPacket** (ref proc-queue buf))
              (len::int (ref proc-queue size))
              (proc-packet::ProcPacket* (aref buf (ref proc-queue start))))
         (set! (aref buf (ref proc-queue start)) NULL
               (ref proc-queue start) (% (+ (ref proc-queue start) 1) len))
         (return proc-packet)))))

  (define-cfn run-queued-procs (available-ticks::Uint32)
    ::void
    (let* ((start-ticks::Uint32 (SDL_GetTicks)))
      (loop
       (let* ((proc-packet::ProcPacket* (dequeue-proc!)))
         (cond
           ((== proc-packet NULL)
            (return))
           (else
            (let* ((proc (-> proc-packet proc))
                   (eval-packet::ScmEvalPacket* (-> proc-packet eval-packet)))
              (cond
                (eval-packet
                 (SDL_LockMutex (-> proc-packet lock))
                 (Scm_Apply proc SCM_NIL eval-packet)
                 (SDL_CondSignal (-> proc-packet cond))
                 (SDL_UnlockMutex (-> proc-packet lock)))
                (else
                 (Scm_ApplyRec0 proc)))))))
       (when (< available-ticks (- (SDL_GetTicks) start-ticks))
         (return)))))

  (define-cfn process-all-events! ()
    ::ScmObj
    (let* ((events SCM_NIL)
           (sdl-event::SDL_Event))
      (while (SDL_PollEvent (& sdl-event))
        (case (ref sdl-event type)
          ((SDL_WINDOWEVENT)
           (case (ref sdl-event window event)
             ((SDL_WINDOWEVENT_MOVED SDL_WINDOWEVENT_RESIZED SDL_WINDOWEVENT_SIZE_CHANGED)
              (set! events (Scm_Cons (SCM_LIST5 (SCM_MAKE_INT (ref sdl-event window windowID))
                                                (window-event->symbol (ref sdl-event window event))
                                                (Scm_MakeIntegerU (ref sdl-event window timestamp))
                                                (SCM_MAKE_INT (ref sdl-event window data1))
                                                (SCM_MAKE_INT (ref sdl-event window data2)))
                                     events))
              ;; Compute transform param when the window is resized.
              (when (== (ref sdl-event window event) SDL_WINDOWEVENT_RESIZED)
                (let* ((target-gwin::GrvWindow* NULL))
                  (for-each (lambda (win)
                              (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
                                (when (== (SDL_GetWindowID (-> gwin window)) (ref sdl-event window windowID))
                                  (set! target-gwin gwin)
                                  (break))))
                            grv-windows)
                  (when target-gwin
                    (compute-transform-param (& (-> target-gwin param))
                                             (cast int (ref sdl-event window data1)) ; width
                                             (cast int (ref sdl-event window data2)) ; height
                                             (ref (-> target-gwin param) left)
                                             (ref (-> target-gwin param) top)
                                             (ref (-> target-gwin param) right)
                                             (ref (-> target-gwin param) bottom))))))
             (else
              (set! events (Scm_Cons (SCM_LIST3 (SCM_MAKE_INT (ref sdl-event window windowID))
                                                (window-event->symbol (ref sdl-event window event))
                                                (Scm_MakeIntegerU (ref sdl-event window timestamp)))
                                     events)))))
          ((SDL_KEYDOWN SDL_KEYUP)
           (set! events (Scm_Cons (Scm_List (SCM_MAKE_INT (ref sdl-event key windowID))
                                            (?: (== (ref sdl-event type) SDL_KEYDOWN) 'key-down 'key-up)
                                            (Scm_MakeIntegerU (ref sdl-event key timestamp))
                                            (scancode->symbol (ref sdl-event key keysym scancode))
                                            (keycode->symbol (ref sdl-event key keysym sym))
                                            (kmod->symbols (ref sdl-event key keysym mod))
                                            (SCM_MAKE_BOOL (ref sdl-event key repeat))
                                            NULL)
                                  events)))
          ((SDL_TEXTEDITING)
           (set! events (Scm_Cons (Scm_List (SCM_MAKE_INT (ref sdl-event edit windowID))
                                            'text-editing
                                            (Scm_MakeIntegerU (ref sdl-event edit timestamp))
                                            (SCM_MAKE_STR_COPYING (ref sdl-event edit text))
                                            (SCM_MAKE_INT (ref sdl-event edit start))
                                            (SCM_MAKE_INT (ref sdl-event edit length))
                                            NULL)
                                  events)))
          ((SDL_TEXTINPUT)
           (set! events (Scm_Cons (SCM_LIST4 (SCM_MAKE_INT (ref sdl-event text windowID))
                                             'text-input
                                             (Scm_MakeIntegerU (ref sdl-event text timestamp))
                                             (SCM_MAKE_STR_COPYING (ref sdl-event text text)))
                                  events)))
          ((SDL_MOUSEMOTION)
           (set! events (Scm_Cons (Scm_List (SCM_MAKE_INT (ref sdl-event motion windowID))
                                            'mouse-motion
                                            (Scm_MakeIntegerU (ref sdl-event motion timestamp))
                                            (SCM_MAKE_INT (ref sdl-event motion which))
                                            (mouse-button-state->symbols (ref sdl-event motion state))
                                            (SCM_MAKE_INT (ref sdl-event motion x))
                                            (SCM_MAKE_INT (ref sdl-event motion y))
                                            (SCM_MAKE_INT (ref sdl-event motion xrel))
                                            (SCM_MAKE_INT (ref sdl-event motion yrel))
                                            NULL)
                                  events)))
          ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
           (set! events (Scm_Cons (Scm_List (SCM_MAKE_INT (ref sdl-event button windowID))
                                            (?: (== (ref sdl-event type) SDL_MOUSEBUTTONDOWN) 'mouse-button-down 'mouse-button-up)
                                            (Scm_MakeIntegerU (ref sdl-event button timestamp))
                                            (SCM_MAKE_INT (ref sdl-event button which))
                                            (mouse-button->symbol (ref sdl-event button button))
                                            (SCM_MAKE_INT (ref sdl-event button clicks))
                                            (SCM_MAKE_INT (ref sdl-event button x))
                                            (SCM_MAKE_INT (ref sdl-event button y))
                                            NULL)
                                  events)))
          ((SDL_MOUSEWHEEL)
           (set! events (Scm_Cons (Scm_List (SCM_MAKE_INT (ref sdl-event wheel windowID))
                                            'mouse-wheel
                                            (Scm_MakeIntegerU (ref sdl-event wheel timestamp))
                                            (SCM_MAKE_INT (ref sdl-event wheel which))
                                            (SCM_MAKE_INT (ref sdl-event wheel x))
                                            (SCM_MAKE_INT (ref sdl-event wheel y))
                                            (?: (== (ref sdl-event wheel direction) SDL_MOUSEWHEEL_NORMAL) 'normal 'flipped)
                                            NULL)
                                  events)))
          ((SDL_JOYAXISMOTION)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            'joystick-axis-motion
                                            (Scm_MakeIntegerU (ref sdl-event jaxis timestamp))
                                            (SCM_MAKE_INT (ref sdl-event jaxis which))
                                            (SCM_MAKE_INT (ref sdl-event jaxis axis))
                                            (SCM_MAKE_INT (ref sdl-event jaxis value))
                                            NULL)
                                  events)))
          ((SDL_JOYBALLMOTION)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            'joystick-ball-motion
                                            (Scm_MakeIntegerU (ref sdl-event jball timestamp))
                                            (SCM_MAKE_INT (ref sdl-event jball which))
                                            (SCM_MAKE_INT (ref sdl-event jball ball))
                                            (SCM_MAKE_INT (ref sdl-event jball xrel))
                                            (SCM_MAKE_INT (ref sdl-event jball yrel))
                                            NULL)
                                  events)))
          ((SDL_JOYHATMOTION)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            'joystick-hat-motion
                                            (Scm_MakeIntegerU (ref sdl-event jhat timestamp))
                                            (SCM_MAKE_INT (ref sdl-event jhat which))
                                            (SCM_MAKE_INT (ref sdl-event jhat hat))
                                            (hat-position->symbol (ref sdl-event jhat value))
                                            NULL)
                                  events)))
          ((SDL_JOYBUTTONDOWN SDL_JOYBUTTONUP)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            (?: (== (ref sdl-event jbutton type) SDL_JOYBUTTONDOWN)
                                                'joystick-button-down
                                                'joystick-button-up)
                                            (Scm_MakeIntegerU (ref sdl-event jbutton timestamp))
                                            (SCM_MAKE_INT (ref sdl-event jbutton which))
                                            (SCM_MAKE_INT (ref sdl-event jbutton button))
                                            (state->symbol (ref sdl-event jbutton state))
                                            NULL)
                                  events)))
          ((SDL_JOYDEVICEADDED SDL_JOYDEVICEREMOVED)
           (set! events (Scm_Cons (SCM_LIST4 SCM_FALSE
                                             (?: (== (ref sdl-event type) SDL_JOYDEVICEADDED)
                                                 'joystick-device-added
                                                 'joystick-device-removed)
                                             (Scm_MakeIntegerU (ref sdl-event jdevice timestamp))
                                             (SCM_MAKE_INT (ref sdl-event jdevice which)))
                                  events)))
          ((SDL_CONTROLLERAXISMOTION)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            'controller-axis-motion
                                            (Scm_MakeIntegerU (ref sdl-event caxis timestamp))
                                            (SCM_MAKE_INT (ref sdl-event caxis which))
                                            (axis->symbol (ref sdl-event caxis axis))
                                            (SCM_MAKE_INT (ref sdl-event caxis value))
                                            NULL)
                                  events)))
          ((SDL_CONTROLLERBUTTONDOWN SDL_CONTROLLERBUTTONUP)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            (?: (== (ref sdl-event type) SDL_CONTROLLERBUTTONDOWN)
                                                'controller-button-down
                                                'controller-button-up)
                                            (Scm_MakeIntegerU (ref sdl-event cbutton timestamp))
                                            (SCM_MAKE_INT (ref sdl-event cbutton which))
                                            (button->symbol (ref sdl-event cbutton button))
                                            (state->symbol (ref sdl-event cbutton state))
                                            NULL)
                                  events)))
          ((SDL_CONTROLLERDEVICEADDED SDL_CONTROLLERDEVICEREMOVED SDL_CONTROLLERDEVICEREMAPPED)
           (set! events (Scm_Cons (SCM_LIST4 SCM_FALSE
                                             (?: (== (ref sdl-event type) SDL_CONTROLLERDEVICEADDED)
                                                 'controller-device-added
                                                 (?: (== (ref sdl-event type) SDL_CONTROLLERDEVICEREMOVED)
                                                     'controller-device-removed
                                                     'controller-device-remapped))
                                             (Scm_MakeIntegerU (ref sdl-event cdevice timestamp))
                                             (SCM_MAKE_INT (ref sdl-event cdevice which)))
                                  events)))
          ((SDL_AUDIODEVICEADDED SDL_AUDIODEVICEREMOVED)
           (set! events (Scm_Cons (SCM_LIST5 SCM_FALSE
                                             (?: (== (ref sdl-event type) SDL_AUDIODEVICEADDED)
                                                 'audio-device-added
                                                 'audio-device-removed)
                                             (Scm_MakeIntegerU (ref sdl-event adevice timestamp))
                                             (SCM_MAKE_INT (ref sdl-event adevice which))
                                             (SCM_MAKE_BOOL (ref sdl-event adevice iscapture)))
                                  events)))
          ((SDL_QUIT)
           (set! events (Scm_Cons (SCM_LIST3 SCM_FALSE
                                             'quit
                                             (Scm_MakeIntegerU (ref sdl-event quit timestamp)))
                                  events)))
          ((SDL_FINGERMOTION SDL_FINGERDOWN SDL_FINGERUP)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            (?: (== (ref sdl-event type) SDL_FINGERMOTION)
                                                'finger-motion
                                                (?: (== (ref sdl-event type) SDL_FINGERDOWN)
                                                    'finger-down
                                                    'finger-up))
                                            (Scm_MakeIntegerU (ref sdl-event tfinger timestamp))
                                            (Scm_MakeInteger (ref sdl-event tfinger touchId))
                                            (Scm_MakeInteger (ref sdl-event tfinger fingerId))
                                            (Scm_MakeFlonum (ref sdl-event tfinger x))
                                            (Scm_MakeFlonum (ref sdl-event tfinger y))
                                            (Scm_MakeFlonum (ref sdl-event tfinger dx))
                                            (Scm_MakeFlonum (ref sdl-event tfinger dy))
                                            (Scm_MakeFlonum (ref sdl-event tfinger pressure))
                                            NULL)
                                  events)))
          ((SDL_MULTIGESTURE)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            'multi-gesture
                                            (Scm_MakeIntegerU (ref sdl-event mgesture timestamp))
                                            (Scm_MakeInteger (ref sdl-event mgesture touchId))
                                            (Scm_MakeFlonum (ref sdl-event mgesture dTheta))
                                            (Scm_MakeFlonum (ref sdl-event mgesture dDist))
                                            (Scm_MakeFlonum (ref sdl-event mgesture x))
                                            (Scm_MakeFlonum (ref sdl-event mgesture y))
                                            (SCM_MAKE_INT (ref sdl-event mgesture numFingers))
                                            NULL)
                                  events)))
          ((SDL_DOLLARGESTURE SDL_DOLLARRECORD)
           (set! events (Scm_Cons (Scm_List SCM_FALSE
                                            (?: (== (ref sdl-event type) SDL_DOLLARGESTURE)
                                                'dollar-gesture
                                                'dollar-record)
                                            (Scm_MakeIntegerU (ref sdl-event dgesture timestamp))
                                            (Scm_MakeInteger (ref sdl-event dgesture touchId))
                                            (Scm_MakeInteger (ref sdl-event dgesture gestureId))
                                            (SCM_MAKE_INT (ref sdl-event dgesture numFingers))
                                            (Scm_MakeFlonum (ref sdl-event dgesture error))
                                            (Scm_MakeFlonum (ref sdl-event dgesture x))
                                            (Scm_MakeFlonum (ref sdl-event dgesture y))
                                            NULL)
                                  events)))
          ((SDL_DROPFILE SDL_DROPTEXT)
           (set! events (Scm_Cons (SCM_LIST4 (SCM_MAKE_INT (ref sdl-event drop windowID))
                                             (?: (== (ref sdl-event type) SDL_DROPFILE) 'drop-file 'drop-text)
                                             (Scm_MakeIntegerU (ref sdl-event drop timestamp))
                                             (SCM_MAKE_STR_COPYING (ref sdl-event drop file)))
                                  events))
           (SDL_free (ref sdl-event drop file)))
          ((SDL_DROPBEGIN SDL_DROPCOMPLETE)
           (set! events (Scm_Cons (SCM_LIST3 (SCM_MAKE_INT (ref sdl-event drop windowID))
                                             (?: (== (ref sdl-event type) SDL_DROPBEGIN) 'drop-begin 'drop-complete)
                                             (Scm_MakeIntegerU (ref sdl-event drop timestamp)))
                                  events))
           (SDL_free (ref sdl-event drop file)))
          (else
           (cond
             ((== (ref sdl-event type) graviton-event)
              (case (ref sdl-event user code)
                ((GRV_CALL_EVENT_CODE)
                 (enqueue-proc! (cast ProcPacket* (ref sdl-event user data1))))))))
          ) ;; end of case
        )   ;; end of while
      (return events)))

  ;; events must be reverse chronological order.
  (define-cfn collect-window-events (gwin::GrvWindow* all-events)
    ::ScmObj
    (let* ((win-events (SCM_LIST1 (SCM_LIST1 'frame-end)))
           (winid (SCM_MAKE_INT (SDL_GetWindowID (-> gwin window)))))
      (for-each (lambda (event)
                  (when (or (SCM_FALSEP (SCM_CAR event))
                            (SCM_EQ (SCM_CAR event) winid))
                    (set! win-events (Scm_Cons (SCM_CDR event) win-events))))
                all-events)
      (return (Scm_Cons (SCM_LIST1 'frame-start) win-events))))

  (define-cfn window-close-event-exists? (win-events)
    ::bool
    (for-each (lambda (event)
                (when (SCM_EQ (SCM_CAR event) 'window-close)
                  (return true)))
              win-events)
    (return false))

  (define-cfn run-window-handlers ()
    ::void
    (let* ((all-events (process-all-events!))
           (will-close-windows SCM_NIL))
      (for-each (lambda (win)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win))
                         (proc (-> gwin proc)))
                    (unless (SCM_FALSEP (-> gwin events))
                      (set! (-> gwin events) (collect-window-events gwin all-events)))
                    (when (SCM_PROCEDUREP proc)
                      (set! (-> gwin proc) SCM_FALSE)
                      (let* ((arity::int (SCM_PROCEDURE_REQUIRED proc)))
                        (cond
                          ((== arity 0)
                           (Scm_ApplyRec0 proc))
                          ((== arity 1)
                           (Scm_ApplyRec1 proc win))
                          (else
                           (Scm_Error "The arity of window handler must be 0 or 1, but got %d" arity)))))
                    (when (SCM_FALSEP (-> gwin proc))
                      (set! will-close-windows (Scm_Cons win will-close-windows)))))
                grv-windows)
      (for-each (lambda (obj)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj)))
                    (destroy-window gwin)))
                will-close-windows)))

  (define-cfn set-event-loop-status (running?::bool)
    ::void
    (SDL_AtomicLock (& (ref event-loop-status lock)))
    (set! (ref event-loop-status running?) running?)
    (SDL_AtomicUnlock (& (ref event-loop-status lock))))

  (define-cfn event-loop-running? ()
    ::bool
    (let* ((running?::bool))
      (SDL_AtomicLock (& (ref event-loop-status lock)))
      (set! running? (ref event-loop-status running?))
      (SDL_AtomicUnlock (& (ref event-loop-status lock)))
      (return running?)))
  )  ;; end of inline-stub

(define-cproc event-loop-running? ()
  ::<boolean>
  (return (event-loop-running?)))

(define-cproc frame-per-second ()
  ::<int>
  (return frame-per-second))

(define-cproc set-frame-per-second! (fps::<int>)
  ::<void>
  (let* ((t::Uint32 (cast Uint32 (floor (/ 1000.0 fps)))))
    (set! frame-per-second fps
          event-loop-ticks t)))

(define-cproc %start-global-event-loop ()
  ::<void>
  (when (event-loop-running?)
    (return))

  (set-event-loop-status true)
  (let* ((packet::ScmEvalPacket))
    (Scm_EvalCString "(run-repl-if-needed)"
                     (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0))
                     (& packet))
    (unless (SCM_FALSEP (ref packet exception))
      (Scm_Raise (ref packet exception) 0)))
  (SDL_StartTextInput)
  (let* ((run-window-handlers-elapse::Uint32 0)
         (run-queued-procs-elapse::Uint32 0)
         (update-window-contents-elapse::Uint32 0))
    (while (not (SCM_NULLP grv-windows))
      (let* ((t::Uint32 (SDL_GetTicks)))
        (run-window-handlers)
        (set! run-window-handlers-elapse (- (SDL_GetTicks) t)))

      (let* ((t::Uint32 (SDL_GetTicks))
             (available-ticks::Uint32 (?: (< event-loop-ticks (+ run-window-handlers-elapse update-window-contents-elapse))
                                          0
                                          (- (+ run-window-handlers-elapse update-window-contents-elapse) event-loop-ticks))))
        (run-queued-procs available-ticks)
        (set! run-queued-procs-elapse (- (SDL_GetTicks) t)))

      (let* ((t::Uint32 (SDL_GetTicks)))
        (update-window-contents)
        (set! update-window-contents-elapse (- (SDL_GetTicks) t)))

      (let* ((total-elapse::Uint32 (+ run-window-handlers-elapse run-queued-procs-elapse update-window-contents-elapse)))
        (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "run-window-handlers:    %d ms" run-window-handlers-elapse)
        (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "run-queued-procs:       %d ms" run-queued-procs-elapse)
        (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "update-window-contents: %d ms" update-window-contents-elapse)
        (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "total:                  %d ms" total-elapse)
        (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "event-loop-ticks:       %d ms" event-loop-ticks)
        (when (< total-elapse event-loop-ticks)
          (SDL_Delay (- event-loop-ticks total-elapse))))))

  (set-event-loop-status false)

  (let* ((packet::ScmEvalPacket))
    (Scm_EvalCString "(wait-all-repl-terminate)"
                     (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0))
                     (& packet))
    (unless (SCM_FALSEP (ref packet exception))
      (Scm_Raise (ref packet exception) 0))))

(define-cproc call-on-main-thread (proc :key (wait?::<boolean> #t))
  ::<void>
  (let* ((packet::ScmEvalPacket* (SCM_NEW (.type ScmEvalPacket))))
    (cond
      ((== main-thread-id (SDL_ThreadID))
       (Scm_Apply proc SCM_NIL packet))
      ((not wait?)
       (let* ((proc-packet::ProcPacket* (SCM_NEW (.type ProcPacket)))
              (event::SDL_Event))
         (set! (-> proc-packet proc) proc
               (-> proc-packet lock) NULL
               (-> proc-packet cond) NULL
               (-> proc-packet eval-packet) NULL)
         (set! (ref event type) graviton-event
               (ref event user code) GRV_CALL_EVENT_CODE
               (ref event user data1) proc-packet
               (ref event user data2) NULL)
         (SDL_PushEvent (& event))
         (return)))
      (else
       (let* ((proc-packet::ProcPacket* (SCM_NEW (.type ProcPacket)))
              (event::SDL_Event))
         (set! (-> proc-packet proc) proc
               (-> proc-packet lock) (SDL_CreateMutex)
               (-> proc-packet cond) (SDL_CreateCond)
               (-> proc-packet eval-packet) packet)
         (set! (ref event type) graviton-event
               (ref event user code) GRV_CALL_EVENT_CODE
               (ref event user data1) proc-packet
               (ref event user data2) NULL)
         (SDL_LockMutex (-> proc-packet lock))
         (SDL_PushEvent (& event))
         (SDL_CondWait (-> proc-packet cond) (-> proc-packet lock))
         (SDL_UnlockMutex (-> proc-packet lock))
         (SDL_DestroyCond (-> proc-packet cond))
         (SDL_DestroyMutex (-> proc-packet lock)))))
    (cond
      ((SCM_FALSEP (-> packet exception))
       (let* ((i::int)
              (args SCM_NIL))
         (for ((set! i (- (-> packet numResults) 1)) (<= 0 i) (pre-- i))
           (set! args (Scm_Cons (aref (-> packet results) i) args)))
         (SCM_RETURN (Scm_Values args))))
      (else
       (Scm_Raise (-> packet exception) 0)))))

(define (start-global-event-loop)
  (guard (e (else (destroy-all-windows)
                  (raise e)))
    (%start-global-event-loop)))


;;;
;;; Image & Window coordinate utilities
;;;

(define-cproc pixel-size (window-or-image)
  ::(<double> <double>)
  (cond
    ((GRV_IMAGE_P window-or-image)
     (return (image-pixel-width (GRV_IMAGE_PTR window-or-image))
             (image-pixel-height (GRV_IMAGE_PTR window-or-image))))
    ((GRV_WINDOW_P window-or-image)
     (return (window-pixel-width (GRV_WINDOW_PTR window-or-image))
             (window-pixel-height (GRV_WINDOW_PTR window-or-image))))
    (else
     (Scm_Error "<graviton-window> or <graviton-image> required, but got %S" window-or-image))))

(define (pixel-width window-or-image)
  (values-ref (pixel-size window-or-image) 0))

(define (pixel-height window-or-image)
  (values-ref (pixel-size window-or-image) 1))

(define-cproc border-left (window-or-image)
  ::<double>
  (return (-> (get-transform-param window-or-image) left)))

(define-cproc border-top (window-or-image)
  ::<double>
  (return (-> (get-transform-param window-or-image) top)))

(define-cproc border-right (window-or-image)
  ::<double>
  (return (-> (get-transform-param window-or-image) right)))

(define-cproc border-bottom (window-or-image)
  ::<double>
  (return (-> (get-transform-param window-or-image) bottom)))

(define-cproc center-point (window-or-image)
  ::<list>
  (let* ((param::TransformParam* (get-transform-param window-or-image)))
    (return (SCM_LIST2 (Scm_MakeFlonum (/ (+ (-> param left) (-> param right)) 2.0))
                       (Scm_MakeFlonum (/ (+ (-> param top) (-> param bottom)) 2.0))))))

(define-cproc center-x (window-or-image)
  ::<double>
  (let* ((param::TransformParam* (get-transform-param window-or-image)))
    (return (/ (+ (-> param left) (-> param right)) 2.0))))

(define-cproc center-y (window-or-image)
  ::<double>
  (let* ((param::TransformParam* (get-transform-param window-or-image)))
    (return (/ (+ (-> param top) (-> param bottom)) 2.0))))

(define (set-border! window-or-image v0 :optional (v1 #f) (v2 #f) (v3 #f))
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
    (cond
      ((window? window-or-image)
       (set-window-border! window-or-image top right bottom left))
      ((image? window-or-image)
       (set-image-border! window-or-image top right bottom left))
      (else
       (errorf "<graviton-window> or <graviton-image> required, but got %S" window-or-image)))))

(define (border-min-x window-or-image)
  (min (border-left window-or-image) (border-right window-or-image)))

(define (border-max-x window-or-image)
  (max (border-left window-or-image) (border-right window-or-image)))

(define (border-min-y window-or-image)
  (min (border-top window-or-image) (border-bottom window-or-image)))

(define (border-max-y window-or-image)
  (max (border-top window-or-image) (border-bottom window-or-image)))


;;;
;;; Draw
;;;

(inline-stub
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
    (update-rect gimage (-> area x) (-> area y) (-> area w) (-> area h)))

  (define-cfn fill-rect (gimage::GrvImage* x0::int y0::int x1::int y1::int color::Uint32)
    ::void
    (let* ((rect::SDL_Rect))
      (set! (ref rect x) (?: (< x0 x1) x0 x1)
            (ref rect y) (?: (< y0 y1) y0 y1)
            (ref rect w) (+ (abs (- x0 x1)) 1)
            (ref rect h) (+ (abs (- y0 y1)) 1))
      (when (!= (SDL_FillRect (-> gimage surface) (& rect) color) 0)
        (Scm_Error "SDL_FillRect failed: %s" (SDL_GetError)))
      (update-rect gimage (ref rect x) (ref rect y) (ref rect w) (ref rect h))))

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
    (update-rect gimage (?: (< x0 x1) x0 x1) (?: (< y0 y1) y0 y1) (+ (abs (- x0 x1)) 1) (+ (abs (- y0 y1)) 1)))
  )  ;; end of inline-stub

(define-cproc %draw-rect (gimage::<graviton-image> point0::<list> point1::<list> color::<int> fill?::<boolean>)
  ::<void>
  (let* ((x0::double (Scm_GetDouble (Scm_ListRef point0 0 SCM_UNBOUND)))
         (y0::double (Scm_GetDouble (Scm_ListRef point0 1 SCM_UNBOUND)))
         (x1::double (Scm_GetDouble (Scm_ListRef point1 0 SCM_UNBOUND)))
         (y1::double (Scm_GetDouble (Scm_ListRef point1 1 SCM_UNBOUND)))
         (ix0::int)
         (iy0::int)
         (ix1::int)
         (iy1::int))
    (image-coordinate gimage x0 y0 (& ix0) (& iy0))
    (image-coordinate gimage x1 y1 (& ix1) (& iy1))
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
       (image-coordinate gimage x y (& ix) (& iy))
       (%%draw-line gimage ix iy ix iy color NULL)))
    (else
     (let* ((x0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 0 SCM_UNBOUND)))
            (y0::double (Scm_GetDouble (Scm_ListRef (SCM_CAR points) 1 SCM_UNBOUND)))
            (ix0::int)
            (iy0::int))
       (image-coordinate gimage x0 y0 (& ix0) (& iy0))
       (for-each (lambda (point)
                   (let* ((x1::double (Scm_GetDouble (Scm_ListRef point 0 SCM_UNBOUND)))
                          (y1::double (Scm_GetDouble (Scm_ListRef point 1 SCM_UNBOUND)))
                          (ix1::int)
                          (iy1::int))
                     (image-coordinate gimage x1 y1 (& ix1) (& iy1))
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
         (image-coordinate gimage x y (& ix) (& iy))
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
         (image-coordinate gimage x0 y0 (& ix0) (& iy0))
         (image-coordinate gimage x1 y1 (& ix1) (& iy1))
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
         (image-coordinate gimage x0 y0 (& ix0) (& iy0))
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
                       (image-coordinate gimage x y (& ix) (& iy))
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

(define (draw-point image point color :key (thickness 0))
  (call-on-main-thread (lambda ()
                         (cond
                           ((= thickness 0)
                            (draw-rect image point point color))
                           (else
                            (draw-circle image point (/. thickness 2) color :fill? #t))))
                       :wait? #f))

(define (draw-rect image point0 point1 color :key (fill? #f) (thickness 0))
  (call-on-main-thread (lambda ()
                         (cond
                           ((= thickness 0)
                            (%draw-rect image point0 point1 color fill?))
                           (else
                            (let ((x0 (point-x point0))
                                  (y0 (point-y point0))
                                  (x1 (point-x point1))
                                  (y1 (point-y point1)))
                              (draw-polygon image
                                            (list (make-point x0 y0)
                                                  (make-point x0 y1)
                                                  (make-point x1 y1)
                                                  (make-point x1 y0))
                                            color
                                            :fill? fill?
                                            :thinkness thickness)))))
                       :wait? #f))

(define (draw-line image points color :key (thickness 0))
  (call-on-main-thread (lambda ()
                         (cond
                           ((= thickness 0)
                            (%draw-line image points color))
                           ((null? points)
                            #f)
                           (else
                            (draw-point image (car points) color :thickness thickness)
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
                                   (draw-point image (make-point x1 y1) color :thickness thickness)
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
                                   (draw-point image (make-point x1 y1) color :thickness thickness)
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
                                   (draw-point image (make-point x1 y1) color :thickness thickness)
                                   (loop (car points) (cdr points)))))))))
                       :wait? #f))

(define (draw-polygon image points color :key (fill? #f) (thickness 0))
  (call-on-main-thread (lambda ()
                         (cond
                           ((= thickness 0)
                            (%draw-polygon image points color fill?))
                           ((null? points)
                            #f)
                           (else
                            (draw-line image (cons (last points) points) color :thickness thickness)
                            (when fill?
                              (%draw-polygon image points color #t)))))
                       :wait? #f))

(define (draw-circle image
                     center-point
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
  (call-on-main-thread (lambda ()
                         (let ((center-x (point-x center-point))
                               (center-y (point-y center-point)))
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
                                    (loop (+ t dt) (cons (rotate-point x y) points)))))))))
                       :wait? #f))


;;;
;;; MessageBox
;;;

(define-cproc %message-box (title::<const-cstring> message::<const-cstring> message-type)
  ::<void>
  (let* ((flags::Uint32))
    (cond
      ((SCM_EQ message-type 'info)
       (set! flags SDL_MESSAGEBOX_INFORMATION))
      ((SCM_EQ message-type 'warning)
       (set! flags SDL_MESSAGEBOX_WARNING))
      ((SCM_EQ message-type 'error)
       (set! flags SDL_MESSAGEBOX_ERROR))
      (else
       (Scm_Error "type must be info, warning or error, but got %S" message-type)))
    (when (< (SDL_ShowSimpleMessageBox flags title message NULL) 0)
      (Scm_Error "SDL_ShowSimpleMessageBox failed: %s" (SDL_GetError)))))

(define (message-box title message :key (type 'info))
  (%message-box title message type))


(include "graviton/enum2sym.scm")

(compile-stub :pkg-config '("sdl2" "SDL2_mixer SDL2_image") :cflags "-g")


(define (next-event window)
  (let1 event (%next-event window)
    (cond
      (event
       event)
      (else
       (shift cont
         (set-window-proc! window
                           (lambda ()
                             (cont (next-event window)))))))))

(define (event-for-each window proc)
  (let loop ()
    (let1 event (next-event window)
      (cond
        ((eof-object? event)
         #f)
        (else
         (proc event)
         (loop))))))

(define (display-new-window title size proc :rest rest)
  (apply make-window title size proc rest)
  (start-global-event-loop))

(define (display-image image :key (fullscreen? #f) (resizable? #f))
  (let ((img-w (image-width image))
        (img-h (image-height image))
        (title (match (command-line)
                 ((program-name args ...)
                  (values-ref (decompose-path program-name) 1))
                 (_
                  "Untitled"))))
    (display-new-window title (if fullscreen?
                                  'fullscreen
                                  (list img-w img-h))
      (lambda (win)
        (let* ((win-w (window-width win))
               (win-h (window-height win))
               (zoom (min (/. win-w img-w) (/. win-h img-h)))
               (sprite (put-image win image (center-x win) (center-y win) :zoom-x zoom :zoom-y zoom))
               (close? #f))
          (event-for-each win
            (match-lambda
              (('window-resized _ w h)
               (set-sprite-zoom! sprite (min (/. w img-w) (/. h img-h))))
              (('window-close _)
               (close-event-stream win))
              (('key-down _ 'escape _ _ _)
               (close-event-stream win))
              (event
               #f)))))
      :resizable? resizable?)))


;;;
;;; REPL
;;;

(define *repl-threads* '())

(define (is-interactive?)
  (let ((gosh? (and (not (null? (command-line)))
                    (equal? (values-ref (decompose-path (car (command-line))) 1) "gosh"))))
    (and gosh? (find-module 'gauche.interactive))))

(define (graviton-read-eval-print-loop in)
  (let1 repl-thread (make-thread
                      (lambda ()
                        (let ((selector (make <selector>))
                              (expr #f)
                              (ready? #f))
                          (define (read-sexpr)
                            (set! ready? #f)
                            (while (not ready?)
                              (let1 count (selector-select selector 500000)
                                (when (and (= count 0) (not (event-loop-running?)))
                                  (set! expr (eof-object))
                                  (set! ready? #t))))
                            expr)
                          (selector-add! selector
                                         in
                                         (lambda (port flag)
                                           (set! expr (read in))
                                           (set! ready? #t))
                                         '(r))
                          (selector-add! selector
                                         in
                                         (lambda (port flag)
                                           (set! expr (eof-object))
                                           (set! ready? #t))
                                         '(x))
                          ((with-module gauche.internal vm-set-current-module) (find-module 'user))
                          ((global-variable-ref (find-module 'gauche.interactive) 'read-eval-print-loop) read-sexpr))))
    (push! *repl-threads* repl-thread)
    (thread-start! repl-thread)))

(define (wait-all-repl-terminate)
  (unwind-protect
      (for-each (^t (thread-join! t)) *repl-threads*)
    (set! *repl-threads* '())))

(define (run-repl-if-needed)
  (when (is-interactive?)
    (graviton-read-eval-print-loop (current-input-port))))


;;;
;;; setter
;;;

(set! (setter window-size) set-window-size!)
(set! (setter window-position) set-window-position!)
(set! (setter window-title) set-window-title!)
(set! (setter window-resizable?) set-window-resizable!)
(set! (setter window-focused?) set-window-focus!)
(set! (setter window-icon) set-window-icon!)
(set! (setter window-maximized?) set-window-maximized!)
(set! (setter window-minimized?) set-window-minimized!)
