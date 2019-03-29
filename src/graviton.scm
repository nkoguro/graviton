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
  (use binary.io)
  (use binary.pack)
  (use data.queue)
  (use file.util)
  (use gauche.collection)
  (use gauche.generator)
  (use gauche.interactive)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.record)
  (use gauche.selector)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.color)
  (use math.const)
  (use parser.peg)
  (use rfc.zlib)
  (use scheme.charset)
  (use srfi-11)
  (use srfi-42)
  (use util.match)

  (export <graviton-window>
          <graviton-image>
          <graviton-tile-image>
          <graviton-sprite>
          <graviton-tile-map>
          <graviton-future>
          <graviton-music>

          async
          async/thread
          async-apply
          async/thread-apply
          await
          force-future
          yield
          await-sleep

          make-window
          destroy-window
          clear-window-sprites!
          send-close-window-event
          window-physical-size
          window-physical-width
          window-physical-height
          set-window-physical-size!
          window-logical-size
          window-logical-width
          window-logical-height
          set-window-logical-size!
          window-fullscreen?
          set-window-fullscreen!
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
          window-icon
          set-window-icon!
          window?
          all-windows
          last-window
          set-window-handler!
          reflect-resized-window-parameter

          frame-per-second
          set-frame-per-second!
          set-global-handler!
          grv-main
          grv-begin
          grv-exit
          grv-repl

          on-window-shown
          on-window-hidden
          on-window-exposed
          on-window-moved
          on-window-resized
          on-window-size-changed
          on-window-minimized
          on-window-maximized
          on-window-restored
          on-window-enter
          on-window-leave
          on-window-focus-gained
          on-window-focus-lost
          on-window-close
          on-window-take-focus
          on-window-hit-test
          on-key-down
          on-key-up
          on-text-editing
          on-text-input
          on-mouse-motion
          on-mouse-button-down
          on-mouse-button-up
          on-mouse-wheel
          on-drop-file
          on-drop-text
          on-drop-begin
          on-drop-complete
          on-update

          on-joystick-axis-motion
          on-joystick-ball-motion
          on-joystick-hat-motion
          on-joystick-button-down
          on-joystick-button-up
          on-joystick-device-added
          on-joystick-device-removed
          on-controller-axis-motion
          on-controller-button-down
          on-controller-button-up
          on-controller-device-added
          on-controller-device-removed
          on-controller-device-remapped
          on-audio-device-added
          on-audio-device-removed
          on-quit
          on-finger-motion
          on-finger-down
          on-finger-up
          on-multi-gesture
          on-dollar-gesture
          on-dollar-record

          display-image
          make-image
          load-image
          save-image
          image-rgba-pixels
          set-image-rgba-pixels!
          bitblt
          divide-image

          make-sprite
          set-sprite-image!
          sprite-image
          set-sprite-x!
          sprite-x
          set-sprite-y!
          sprite-y
          set-sprite-z!
          sprite-z
          set-sprite-angle!
          sprite-angle
          set-sprite-zoom!
          sprite-zoom
          set-sprite-visible!
          sprite-visible?
          set-sprite-color!
          sprite-color
          set-sprite-clip!
          sprite-clip
          sprite-clip-x
          sprite-clip-y
          sprite-clip-width
          sprite-clip-height

          make-tile-map
          tile-map-tile-index
          tile-map-foreground-color
          tile-map-background-color
          set-tile-map-tile!
          tile-map-offset
          set-tile-map-offset!
          tile-map-sprite
          tile-map-tile-images
          tile-map-columns
          tile-map-rows
          tile-map->output-port

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

          play-mml
          beep
          save-mml
          load-music
          play-music
          stop-music
          pause-music
          resume-music
          playing-music?
          paused-music?
          set-music-volume!
          music-volume
          load-sound
          play-sound
          stop-sound
          pause-sound
          resume-sound
          playing-sound?
          paused-sound?
          set-sound-volume!
          sound-volume

          rgb
          rgba
          color
          register-color!
          define-color
          ) ;; end of export
  ) ;; end of define-module

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

  (define-ctype GrvTileImage::(.struct
                               (image
                                rect::SDL_Rect)))

  (define-ctype GrvSprite::(.struct
                            (window
                             image
                             srcrect::SDL_Rect*
                             center-x::double
                             center-y::double
                             z::double
                             angle::double
                             zoom-x::double
                             zoom-y::double
                             visible::bool
                             color::Uint32
                             clip::SDL_Rect*)))

  (define-ctype GrvWindow::(.struct
                            (window::SDL_Window*
                             renderer::SDL_Renderer*
                             sprites
                             icon
                             logical-width::int
                             logical-height::int
                             offset-x::int
                             offset-y::int
                             handler-table
                             clip::SDL_Rect*)))

  (define-ctype ScratchArea::(.struct
                              (x::int
                               y::int
                               w::int
                               h::int
                               data::char*)))

  (define-ctype GrvAttribute::(.struct
                               (foreground-color::Uint32
                                background-color::Uint32)))

  (define-ctype GrvTileMap::(.struct
                             (tiles::Uint32*
                              attrs::GrvAttribute**
                              buf-tiles::Uint32*
                              buf-attrs::GrvAttribute**
                              columns::int
                              rows::int
                              offset::int
                              image
                              tile-images
                              tile-width::int
                              tile-height::int
                              sprite)))

  (define-ctype EventLoopStatus::(.struct
                                  (lock::SDL_SpinLock
                                   running?::bool)))

  (define-ctype GrvFuture::(.struct
                            (lock::SDL_mutex*
                             cond::SDL_cond*
                             result
                             exception
                             message::char*
                             continuations
                             consumed?::bool)))
  "
typedef enum {
    SOUNDLET_TONE = 1,
    SOUNDLET_COMPOSITE = 2,
} SoundletType;

typedef enum {
    TONE_SILENT = 0,
    TONE_SINE = 1,
    TONE_SQUARE50 = 2,
    TONE_SQUARE12 = 3,
    TONE_SQUARE25 = 4,
    TONE_TRIANGLE = 5,
    TONE_SAWTOOTH = 6,
    TONE_LONG_NOISE = 7,
    TONE_SHORT_NOISE = 8,
} ToneType;

struct GrvSoundletRec;

typedef struct {
    ToneType type;
    double *freqs;
    double *amps;
    int num_freqs;
    double left_volume;
    double right_volume;
    int attack_time;   // sec * 44100
    int decay_time;    // sec * 44100
    double sustain_level;
    int release_time;  // sec * 44100
} GrvToneSoundlet;

typedef struct {
    struct GrvSoundletRec **children;
    int num_children;
} GrvCompositeSoundlet;

typedef struct GrvSoundletRec {
    struct GrvSoundletRec *next;
    SoundletType type;
    int length;
    union {
        GrvToneSoundlet* tone;
        GrvCompositeSoundlet* composite;
    } data;
} GrvSoundlet;
"
  (define-ctype GrvSoundletContext::(.struct
                                     (start-position::int
                                      soundlet::GrvSoundlet*)))

  (define-ctype GrvMMLMusicContext::(.struct
                                     (position::int
                                      soundlet-contexts::GrvSoundletContext**
                                      num-soundlet-contexts::int
                                      future)))

  (define-ctype GrvMMLMusicContextQueue::(.struct
                                          (buf::GrvMMLMusicContext**
                                           length::int
                                           start::int
                                           end::int)))

  (define-ctype GrvMusic::(.struct
                           (music::Mix_Music*)))

  (define-ctype GrvMusicContext::(.struct
                                  (music
                                   future)))

  (define-ctype GrvSound::(.struct
                           (chunk::Mix_Chunk*)))

  (define-ctype GrvSoundContext::(.struct
                                  (sound
                                   future)))

  ) ;; end of declcode

 (define-cvar main-thread-id::SDL_threadID :static)
 (define-cvar grv-windows :static SCM_NIL)
 (define-cvar event-loop-status::EventLoopStatus)
 (define-cvar frame-per-second::int :static 30)
 (define-cvar global-handler-table :static)
 (define-cvar graviton-event-type::Uint32 :static)
 (define-cvar graviton-module :static)
 (define-cvar mml-music-context-queue::GrvMMLMusicContextQueue :static)
 (define-cvar mml-paused?::bool :static)
 (define-cvar playing-music-context::GrvMusicContext* :static)
 (define-cvar noise-table::double* :static)
 (define-cvar music-last-finished-tick::Uint32 :static)
 (define-cvar playing-sound-contexts::GrvSoundContext** :static)
 (define-cvar global-lock::SDL_SpinLock :static)
 (define-cvar main-thunk-finished?::bool :static)

 (define-cptr <graviton-window> :private
   "GrvWindow*" "GravitonWindowClass" "GRV_WINDOW_P" "MAKE_GRV_WINDOW" "GRV_WINDOW_PTR")

 (define-cptr <graviton-image> :private
   "GrvImage*" "GravitonImageClass" "GRV_IMAGE_P" "MAKE_GRV_IMAGE" "GRV_IMAGE_PTR")

 (define-cptr <graviton-tile-image> :private
   "GrvTileImage*" "GravitonTileImageClass" "GRV_TILE_IMAGE_P" "MAKE_GRV_TILE_IMAGE" "GRV_TILE_IMAGE_PTR")

 (define-cptr <graviton-texture> :private
   "GrvTexture*" "GravitonTextureClass" "GRV_TEXTURE_P" "MAKE_GRV_TEXTURE" "GRV_TEXTURE_PTR")

 (define-cptr <graviton-sprite> :private
   "GrvSprite*" "GravitonSpriteClass" "GRV_SPRITE_P" "MAKE_GRV_SPRITE" "GRV_SPRITE_PTR")

 (define-cptr <graviton-tile-map> :private
   "GrvTileMap*" "GravitonTileMapClass" "GRV_TILE_MAP_P" "MAKE_GRV_TILE_MAP" "GRV_TILE_MAP_PTR")

 (define-cptr <graviton-future> :private
   "GrvFuture*" "GravitonFutureClass" "GRV_FUTURE_P" "MAKE_GRV_FUTURE" "GRV_FUTURE_PTR")

 (define-cptr <graviton-soundlet> :private
   "GrvSoundlet*" "GravitonSoundletClass" "GRV_SOUNDLET_P" "MAKE_GRV_SOUNDLET" "GRV_SOUNDLET_PTR")

 (define-cptr <graviton-music> :private
   "GrvMusic*" "GravitonMusicClass" "GRV_MUSIC_P" "MAKE_GRV_MUSIC" "GRV_MUSIC_PTR")

 (define-cptr <graviton-sound> :private
   "GrvSound*" "GravitonSoundClass" "GRV_SOUND_P" "MAKE_GRV_SOUND" "GRV_SOUND_PTR")

 (.define GRAVITON_EXCEPTION_CODE 1)
 (.define GRAVITON_UNCAUGHT_EXCEPTION_CODE 2)
 (.define GRAVITON_MML_FINISH_CODE 3)
 (.define GRAVITON_APPLY_CODE 5)
 (.define GRAVITON_UPDATE_WINDOWS 6)

 (.define MML_MUSIC_CONTEXT_INITIAL_LENGTH 16)
 (.define NOISE_TABLE_SIZE 32768)
 (.define CHANNEL_SIZE 16)

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
   (when (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 2048)
     (Scm_Error "Mix_OpenAudio failed: %s" (Mix_GetError)))
   (IMG_Init (logior IMG_INIT_JPG IMG_INIT_PNG IMG_INIT_TIF))

   (Scm_AddCleanupHandler teardown-libs NULL)

   (set! graviton-event-type (SDL_RegisterEvents 1))
   (when (== graviton-event-type #xffffffff)
     (Scm_Error "SDL_RegisterEvents failed: %s" (SDL_GetError)))

   (set! main-thread-id (SDL_ThreadID)
         global-handler-table (Scm_MakeHashTableSimple SCM_HASH_EQ 16)
         graviton-module (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0)))
   (set! (ref event-loop-status lock) 0
         (ref event-loop-status running?) false)

   (set! (ref mml-music-context-queue buf) (SCM_NEW_ARRAY (.type GrvMMLMusicContext*) MML_MUSIC_CONTEXT_INITIAL_LENGTH)
         (ref mml-music-context-queue length) MML_MUSIC_CONTEXT_INITIAL_LENGTH
         (ref mml-music-context-queue start) 0
         (ref mml-music-context-queue end) 0
         mml-paused? false)
   (dotimes (i (ref mml-music-context-queue length))
     (set! (aref (ref mml-music-context-queue buf) i) NULL))

   (set! noise-table (SCM_NEW_ATOMIC_ARRAY (.type double) NOISE_TABLE_SIZE))
   (dotimes (i NOISE_TABLE_SIZE)
     (set! (aref noise-table i) (* (- (/ (cast double (random)) RAND_MAX) 0.5) 2.0)))

   (set! music-last-finished-tick 0
         playing-music-context NULL)
   (Mix_HookMusicFinished finish-music)

   (Mix_AllocateChannels CHANNEL_SIZE)
   (set! playing-sound-contexts (SCM_NEW_ARRAY (.type GrvSoundContext*) CHANNEL_SIZE))
   (Mix_ChannelFinished finish-sound)

   (set! global-lock 0)
   ) ;; end of initialize-libs

 (initcode
  (initialize-libs))
 ) ;; end of inline-stub

;;;
;;; Common utilities
;;;

(inline-stub
 (define-cfn lock-global-var ()
   ::void
   (SDL_AtomicLock (& global-lock)))

 (define-cfn unlock-global-var ()
   ::void
   (SDL_AtomicUnlock (& global-lock)))

 (define-cfn main-loop-apply (proc args)
   ::void
   (let* ((event::SDL_Event))
     (set! (ref event type) graviton-event-type
           (ref event user code) GRAVITON_APPLY_CODE
           (ref event user data1) proc
           (ref event user data2) args)
     (SDL_PushEvent (& event))))
 ) ;; end of inline-stub

(define-cproc current-ticks ()
  ::<int>
  (return (SDL_GetTicks)))

;;;
;;;
;;;

(include "enum2sym.scm")
(include "scheduler.scm")
(include "async.scm")
(include "image.scm")
(include "sprite.scm")
(include "tilemap.scm")
(include "window.scm")
(include "event.scm")
(include "messagebox.scm")
(include "draw.scm")
(include "png.scm")
(include "music.scm")
(include "sound.scm")
(include "repl.scm")

;;;
;;; setter
;;;

(set! (setter window-fullscreen?) set-window-fullscreen!)
(set! (setter window-position) set-window-position!)
(set! (setter window-title) set-window-title!)
(set! (setter window-resizable?) set-window-resizable!)
(set! (setter window-icon) set-window-icon!)
(set! (setter window-maximized?) set-window-maximized!)
(set! (setter window-minimized?) set-window-minimized!)

(set! (setter sprite-image) set-sprite-image!)
(set! (setter sprite-x) set-sprite-x!)
(set! (setter sprite-y) set-sprite-y!)
(set! (setter sprite-z) set-sprite-z!)
(set! (setter sprite-angle) set-sprite-angle!)
(set! (setter sprite-zoom) set-sprite-zoom!)
(set! (setter sprite-visible?) set-sprite-visible!)
(set! (setter sprite-color) set-sprite-color!)

(set! (setter tile-map-offset) set-tile-map-offset!)

;;;
;;; Misc
;;;

(define (grv-main thunk)
  (cond
    ((event-loop-running?)
     (thunk))
    (else
     (guard (e (else (destroy-all-windows)
                     (kill-scheduler)
                     (raise e)))
       (run-scheduler)
       (set-main-thunk-finished? #f)
       (start-global-event-loop (lambda ()
                                  (unwind-protect
                                      (thunk)
                                    (set-main-thunk-finished? #t))))
       (shutdown-scheduler)))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_)
     (grv-main (lambda () #f)))
    ((_ expr ...)
     (grv-main (lambda () expr ...)))))

(define (grv-exit)
  (destroy-all-windows))

(define (display-image image :key (fullscreen? #f) (resizable? #f))
  (grv-begin
    (let* ((title (match (command-line)
                    ((program-name args ...)
                     (values-ref (decompose-path program-name) 1))
                    (_
                     "Untitled")))
           (win (make-window (match (command-line)
                               ((program-name args ...)
                                (values-ref (decompose-path program-name) 1))
                               (_
                                "Untitled"))
                             (image-width image)
                             (image-height image)
                             :resizable? resizable?
                             :fullscreen? fullscreen?))
           (sprite (make-sprite win :image image :x (center-x win) :y (center-y win))))
      (on-key-up win (scancode sym mod repeat?)
        (case scancode
          ((escape)
           (destroy-window win))
          ((f)
           (set-window-fullscreen! win (not (window-fullscreen? win))))))
      (on-text-input win (text)
        (cond
          ((equal? text "+")
           (let ((w (* (window-physical-width win) 2))
                 (h (* (window-physical-height win) 2)))
             (set-window-physical-size! win w h)))
          ((equal? text "-")
           (let ((w (/ (window-physical-width win) 2))
                 (h (/ (window-physical-height win) 2)))
             (when (and (<= (image-width image) w) (<= (image-height image) h))
               (set-window-physical-size! win w h)))))))))

(define-method center-point ((window <graviton-window>))
  (window-center-point window))

(define-method center-point ((image <graviton-image>))
  (image-center-point image))

(define (center-x window-or-image)
  (list-ref (center-point window-or-image) 0))

(define (center-y window-or-image)
  (list-ref (center-point window-or-image) 1))

