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
  (use gauche.parameter)
  (use gauche.partcont)
  (use util.match)

  (export <graviton-window>
          <graviton-image>
          <graviton-sprite>

          call-with-window
          set-window-resolution!
          close-window

          match-events
          load-image
          put-image
          image-rgba-pixels
          set-image-rgba-pixels!
          sprite-center-position
          set-sprite-center-position!
          ))

(select-module graviton)

(inline-stub
  (declcode
   (.include "SDL.h"
             "SDL_image.h"
             "SDL_mixer.h"
             "gauche.h"
             "gauche/number.h"
             "gauche/vector.h"
             "stdbool.h"
             "stdio.h"
             "string.h")
   "
typedef struct {
    SDL_Surface *surface;
    SDL_Rect update_rect;
    SDL_Texture *texture;
} GrvImage;

typedef struct {
    ScmObj window;
    ScmObj image;
    SDL_Rect srcrect;
    double center_x;
    double center_y;
    double z;
    double angle;
    double zoom_x;
    double zoom_y;
    SDL_RendererFlip flip;
    bool visible;
} GrvSprite;

typedef struct {
    SDL_Window* window;
    double window_center_x;
    double window_center_y;
    double screen_center_x;
    double screen_center_y;
    double zoom;
    SDL_Renderer* renderer;
    ScmObj proc;
    ScmObj events;
    ScmObj sprites;
} GrvWindow;
"
   "static ScmObj grv_windows = SCM_NIL;"
   "static bool running_event_loop = false;"
   ) ;; end of declcode

  (define-cptr <graviton-window> :private
    "GrvWindow*" "GravitonWindowClass" "GRV_WINDOW_P" "MAKE_GRV_WINDOW" "GRV_WINDOW_PTR")

  (define-cptr <graviton-image> :private
    "GrvImage*" "GravitonImageClass" "GRV_IMAGE_P" "MAKE_GRV_IMAGE" "GRV_IMAGE_PTR")

  (define-cptr <graviton-sprite> :private
    "GrvSprite*" "GravitonSpriteClass" "GRV_SPRITE_P" "MAKE_GRV_SPRITE" "GRV_SPRITE_PTR")

  (define-cfn teardown-libs (data::|void*|)
    ::void :static
    (Mix_CloseAudio)
    (Mix_Quit)

    (SDL_Quit))

  (define-cfn initialize-libs ()
    ::void :static
    (SDL_Init (logior SDL_INIT_VIDEO SDL_INIT_AUDIO))
    (Mix_Init (logior MIX_INIT_FLAC MIX_INIT_MOD MIX_INIT_MP3 MIX_INIT_OGG))
    (when (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 1024)
      (Scm_Error "Mix_OpenAudio failed: %s" (Mix_GetError)))
    (IMG_Init (logior IMG_INIT_JPG IMG_INIT_PNG IMG_INIT_TIF))

    (Scm_AddCleanupHandler teardown-libs NULL))

  (initcode
   (initialize-libs))


  (define-cfn register-grv-window (gwin::GrvWindow*)
    ::ScmObj :static
    (let* ((obj (MAKE_GRV_WINDOW gwin)))
      (set! grv_windows (Scm_Cons obj grv_windows))
      (return obj)))

  (define-cfn unregister-grv-window (gwin::GrvWindow*)
    ::void :static
    (let* ((prev SCM_NIL))
      (pair-for-each (lambda (pair)
                       (let* ((curwin::GrvWindow* (GRV_WINDOW_PTR (SCM_CAR pair))))
                         (when (== gwin curwin)
                           (cond
                             ((SCM_NULLP prev)
                              (set! grv_windows (SCM_CDR pair))
                              (break))
                             (else
                              (SCM_SET_CDR prev (SCM_CDR pair))
                              (break))))))
                     grv_windows)))
  )  ;; end of inline-stub

(define-cproc %create-window (title::<const-cstring> size)
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
       (set! flags SDL_WINDOW_FULLSCREEN))
      (else
       (Scm_Error "size must be a list of two integer elements or 'fullscreen, but got %S" size)))
    (let* ((gwin::GrvWindow* (SCM_NEW (.type GrvWindow))))
      (set! (-> gwin window) NULL
            (-> gwin renderer) NULL
            (-> gwin proc) SCM_FALSE
            (-> gwin events) SCM_NIL
            (-> gwin sprites) SCM_NIL)

      (set! (-> gwin window) (SDL_CreateWindow title
                                               SDL_WINDOWPOS_UNDEFINED
                                               SDL_WINDOWPOS_UNDEFINED
                                               width
                                               height
                                               flags))
      (unless (-> gwin window)
        (Scm_Error "SDL_CreateWindow failed: %s" (SDL_GetError)))

      (let* ((w::int)
             (h::int))
        (SDL_GetWindowSize (-> gwin window) (& w) (& h))
        (set! (-> gwin window_center_x) (/ w 2.0)
              (-> gwin window_center_y) (/ h 2.0)
              (-> gwin screen_center_x) (-> gwin window_center_x)
              (-> gwin screen_center_y) (-> gwin window_center_y)
              (-> gwin zoom) 1.0))

      (set! (-> gwin renderer) (SDL_CreateRenderer (-> gwin window) -1 SDL_RENDERER_PRESENTVSYNC))
      (unless (-> gwin renderer)
        (Scm_Error "SDL_CreateRenderer failed: %s" (SDL_GetError)))

      (return (register-grv-window gwin)))))

(define-cproc set-window-resolution! (gwin::<graviton-window> w::<int> h::<int>)
  ::<void>
  (let* ((screen_center_x::double (/ w 2.0))
         (screen_center_y::double (/ h 2.0))
         (zx::double (/ (-> gwin window_center_x) screen_center_x))
         (zy::double (/ (-> gwin window_center_y) screen_center_y))
         (zoom::double (?: (< zx zy) zx zy))
         (rect::SDL_Rect))
    (set! (-> gwin screen_center_x) screen_center_x
          (-> gwin screen_center_y) screen_center_y
          (-> gwin zoom) zoom)
    (set! (ref rect w) (cast int (* w zoom))
          (ref rect h) (cast int (* h zoom))
          (ref rect x) (cast int (- (-> gwin window_center_x) (/ (ref rect w) 2.0)))
          (ref rect y) (cast int (- (-> gwin window_center_y) (/ (ref rect h) 2.0))))
    (SDL_RenderSetClipRect (-> gwin renderer) (& rect))))

(load "graviton/enum2sym.scm")

(inline-stub
  (define-cfn get-events ()
    ::ScmObj :static
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
                                     events)))
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
           (SDL_free (ref sdl-event drop file)))))
      (return events)))

  ;; events must be reverse chronological order.
  (define-cfn extract-window-events (gwin::GrvWindow* all-events)
    ::ScmObj :static
    (let* ((win-events SCM_NIL)
           (winid (SCM_MAKE_INT (SDL_GetWindowID (-> gwin window)))))
      (for-each (lambda (event)
                  (when (or (SCM_FALSEP (SCM_CAR event))
                            (SCM_EQ (SCM_CAR event) winid))
                    (set! win-events (Scm_Cons (SCM_CDR event) win-events))))
                all-events)
      (return win-events)))

  (define-cfn window-close-event-exists? (win-events)
    ::bool :static
    (for-each (lambda (event)
                (when (SCM_EQ (SCM_CAR event) 'window-close)
                  (return true)))
              win-events)
    (return false))

  (define-cfn create-streaming-texture-from-surface (renderer::SDL_Renderer* surface::SDL_Surface*)
    ::SDL_Texture* :static
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
    ::void :static
    (let* ((pixels::void*)
           (pitch::int)
           (y::int))
      (when (< (SDL_LockSurface surface) 0)
        (Scm_Error "SDL_LockSurface failed: %s" (SDL_GetError)))
      (when (< (SDL_LockTexture texture rect (& pixels) (& pitch)) 0)
        (Scm_Error "SDL_LockTexture failed: %s" (SDL_GetError)))
      (for ((set! y (-> rect y)) (< y (-> rect h)) (pre++ y))
        (memcpy (+ pixels (* y pitch))
                (+ (-> surface pixels) (* y (-> surface pitch)) (* (-> rect x) (-> surface format BytesPerPixel)))
                pitch))
      (SDL_UnlockTexture texture)
      (SDL_UnlockSurface surface)))

  (define-cfn get-texture (gwin::GrvWindow* image::GrvImage*)
    ::SDL_Texture* :static
    (cond
      ((== (-> image texture) NULL)
       (set! (-> image texture) (create-streaming-texture-from-surface (-> gwin renderer) (-> image surface)))
       (when (== (-> image texture) NULL)
         (Scm_Error "SDL_CreateTextureFromSurface failed: %s" (SDL_GetError)))
       (SDL_SetTextureBlendMode (-> image texture) SDL_BLENDMODE_BLEND))
      ((not (SDL_RectEmpty (& (-> image update_rect))))
       (let* ((format::Uint32)
              (access::int)
              (w::int)
              (h::int))
         (when (< (SDL_QueryTexture (-> image texture) (& format) (& access) (& w) (& h)) 0)
           (Scm_Error "SDL_QueryTexture failed: %s" (SDL_GetError)))
         (cond
           ((!= access SDL_TEXTUREACCESS_STREAMING)
            (SDL_DestroyTexture (-> image texture))
            (set! (-> image texture) (create-streaming-texture-from-surface (-> gwin renderer) (-> image surface))))
           (else
            (update-texture (-> image texture) (-> image surface) (& (-> image update_rect))))))))
    (set! (ref (-> image update_rect) x) 0
          (ref (-> image update_rect) y) 0
          (ref (-> image update_rect) w) 0
          (ref (-> image update_rect) h) 0)
    (return (-> image texture)))

  (define-cfn remove-window-sprite (sprite)
    ::void :static
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
    ::void :static
    (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR (-> (GRV_SPRITE_PTR sprite) window))))
      (cond
        ((SCM_NULLP (-> gwin sprites))
         (set! (-> gwin sprites) (SCM_LIST1 sprite)))
        (else
         (pair-for-each (lambda (pair)
                          (cond
                            ((> (-> (GRV_SPRITE_PTR (SCM_CAR pair)) z) (-> (GRV_SPRITE_PTR sprite) z))
                             (SCM_SET_CDR pair (Scm_Cons (SCM_CAR pair) (SCM_CDR pair)))
                             (SCM_SET_CAR pair sprite)
                             (break))
                            ((SCM_NULLP (SCM_CDR pair))
                             (SCM_SET_CDR pair (Scm_Cons sprite SCM_NIL))
                             (break))))
                        (-> gwin sprites))))))
  )  ;; end of inline-stub


(define-cproc create-image (w::<int> h::<int>)
  ::<graviton-image>
  (let* ((surface::SDL_Surface* (SDL_CreateRGBSurfaceWithFormat 0 w h 32 SDL_PIXELFORMAT_RGBA32))
         (image::GrvImage* (SCM_NEW (.type GrvImage))))
    (when (== surface NULL)
      (Scm_Error "SDL_CreateRGBSurfaceWithFormat failed: %s" (SDL_GetError)))
    (SDL_SetSurfaceBlendMode surface SDL_BLENDMODE_BLEND)
    (set! (-> image surface) surface
          (-> image texture) NULL
          (ref (-> image update_rect) x) 0
          (ref (-> image update_rect) y) 0
          (ref (-> image update_rect) w) 0
          (ref (-> image update_rect) h) 0)
    (return image)))

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
            (-> image texture) NULL
            (ref (-> image update_rect) x) 0
            (ref (-> image update_rect) y) 0
            (ref (-> image update_rect) w) 0
            (ref (-> image update_rect) h) 0)
      (return image))))

(define-cproc image-size (image::<graviton-image>)
  ::(<int> <int>)
  (return (-> image surface w) (-> image surface h)))

(define-cproc image-rgba-pixels (image::<graviton-image>)
  (let* ((surface::SDL_Surface* (-> image surface))
         (len::int (* (-> surface w) (-> surface h) 4))
         (vec (Scm_MakeU32Vector len 0)))
    (SDL_LockSurface surface)
    (memcpy (SCM_U32VECTOR_ELEMENTS vec) (-> surface pixels) len)
    (SDL_UnlockSurface surface)
    (return vec)))

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
    (Scm_Error "image mush be <graviton-image> or #f, but got %S" image))
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
    (let* ((sprite-obj (MAKE_GRV_SPRITE sprite)))
      (insert-window-sprite sprite-obj)
      (return sprite-obj))))

(define-cproc %set-sprite-image! (sprite::<graviton-sprite> image rect)
  ::<void>
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image))
    (Scm_Error "image mush be <graviton-image> or #f, but got %S" image))
  (unless (and (SCM_LISTP rect)
               (== (Scm_Length rect) 4)
               (SCM_INTP (Scm_ListRef rect 0 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 1 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 2 SCM_UNBOUND))
               (SCM_INTP (Scm_ListRef rect 3 SCM_UNBOUND)))
    (Scm_Error "rect must be (x y w h), but got %S" rect))
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

(define-cproc set-sprite-zoom! (sprite::<graviton-sprite> zoom-x::<double> zoom-y::<double>)
  ::<void>
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
           (-> sprite flip) SDL_FLIP_NONE))))

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

(define-cproc set-window-proc! (gwin::<graviton-window> proc)
  (set! (-> gwin proc) proc))

(define-cproc get-window-events (gwin::<graviton-window>)
  (return (-> gwin events)))

(define-cproc clear-window-sprites! (gwin::<graviton-window>)
  (set! (-> gwin sprites) SCM_NIL))

(define-cproc run-event-loop ()
  (when running-event-loop
    (return))

  (set! running-event-loop true)
  (SDL_StartTextInput)
  (while (not (SCM_NULLP grv_windows))
    (let* ((all-events (get-events))
           (will-close-windows SCM_NIL))
      (for-each (lambda (obj)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj))
                         (proc (-> gwin proc)))
                    (set! (-> gwin events) (extract-window-events gwin all-events))
                    (when (SCM_PROCEDUREP proc)
                      (set! (-> gwin proc) SCM_FALSE)
                      (Scm_ApplyRec0 proc))
                    (when (window-close-event-exists? (-> gwin events))
                      (set! will-close-windows (Scm_Cons obj will-close-windows)))))
                grv_windows)
      (for-each (lambda (obj)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj)))
                    (unregister-grv-window gwin)
                    (SDL_DestroyRenderer (-> gwin renderer))
                    (SDL_DestroyWindow (-> gwin window))))
                will-close-windows)
      (for-each (lambda (gwin-obj)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR gwin-obj))
                         (renderer::SDL_Renderer* (-> gwin renderer)))
                    (SDL_SetRenderDrawBlendMode renderer SDL_BLENDMODE_BLEND)
                    (SDL_SetRenderDrawColor renderer 0 0 0 255)
                    (SDL_RenderClear renderer)
                    (for-each (lambda (sprite-obj)
                                (let* ((sprite::GrvSprite* (GRV_SPRITE_PTR sprite-obj)))
                                  (when (or (SCM_FALSEP (-> sprite image)) (not (-> sprite visible)))
                                    (continue))
                                  (let* ((texture::SDL_Texture* (get-texture gwin (GRV_IMAGE_PTR (-> sprite image))))
                                         (dstrect::SDL_Rect)
                                         (spr-w::double (* (ref (-> sprite srcrect) w) (-> sprite zoom_x)))
                                         (spr-h::double (* (ref (-> sprite srcrect) h) (-> sprite zoom_y))))
                                    (set! (ref dstrect x) (cast int (+ (* (- (- (-> sprite center_x) (/ spr-w 2))
                                                                             (-> gwin screen_center_x))
                                                                          (-> gwin zoom))
                                                                       (-> gwin window_center_x)))
                                          (ref dstrect y) (cast int (+ (* (- (- (-> sprite center_y) (/ spr-h 2))
                                                                             (-> gwin screen_center_y))
                                                                          (-> gwin zoom))
                                                                       (-> gwin window_center_y)))
                                          (ref dstrect w) (cast int (* spr-w (-> gwin zoom)))
                                          (ref dstrect h) (cast int (* spr-h (-> gwin zoom))))
                                    (SDL_RenderCopyEx renderer
                                                      texture
                                                      (& (-> sprite srcrect))
                                                      (& dstrect)
                                                      (-> sprite angle)
                                                      NULL
                                                      (-> sprite flip)))))
                              (-> gwin sprites))
                    (SDL_RenderPresent renderer)))
                grv_windows)))
  (set! running-event-loop false))

(define-cproc close-window (gwin::<graviton-window>)
  ::<void>
  (let* ((event::SDL_Event))
    (set! (ref event window type) SDL_WINDOWEVENT
          (ref event window windowID) (SDL_GetWindowID (-> gwin window))
          (ref event window event) SDL_WINDOWEVENT_CLOSE)
    (SDL_PushEvent (& event))))

(define (handle-events window handler)
  (shift cont (set-window-proc! window
                                (lambda ()
                                  (for-each (lambda (event)
                                              (handler event))
                                            (get-window-events window))
                                  (cont)))))

(define-syntax match-events
  (syntax-rules ()
    ((_ window clause ...)
     (handle-events window (match-lambda clause ... (else #f))))))

(define (call-with-window title size thunk)
  (let1 window (%create-window title size)
    (reset
      (thunk window)))
  (run-event-loop))

(compile-stub :pkg-config '("sdl2" "SDL2_mixer SDL2_image") :cflags "-g")

(define-method set-sprite-image! ((sprite <graviton-sprite>) (image <graviton-image>))
  (receive (w h) (image-size image)
    (%set-sprite-image! sprite image (list 0 0 w h))))

(define-method set-sprite-image! ((sprite <graviton-sprite>)
                                  (image <graviton-image>)
                                  (rect <list>))
  (%set-sprite-image! sprite image rect))

(define-method put-image ((window <graviton-window>)
                          (image <graviton-image>)
                          (center-x <real>)
                          (center-y <real>)
                          :key
                          rect
                          (z 0)
                          (angle 0)
                          (zoom-x 1.0)
                          (zoom-y 1.0)
                          (visible #t))
  (let1 rect (if (undefined? rect)
                 (receive (w h) (image-size image)
                   (list 0 0 w h))
                 rect)
    (make-sprite window image center-x center-y z rect angle zoom-x zoom-y visible)))
