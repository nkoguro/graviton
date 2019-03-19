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
  (use data.queue)
  (use file.util)
  (use gauche.collection)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.record)
  (use gauche.selector)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.png)
  (use math.const)
  (use parser.peg)
  (use scheme.charset)
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
 (define-cvar repl-thread :static)
 (define-cvar repl-channel :static)
 (define-cvar graviton-event-type::Uint32 :static)
 (define-cvar graviton-module :static)
 (define-cvar mml-music-context-queue::GrvMMLMusicContextQueue :static)
 (define-cvar mml-paused?::bool :static)
 (define-cvar playing-music-context::GrvMusicContext* :static)
 (define-cvar playing-music-lock::SDL_SpinLock :static)
 (define-cvar noise-table::double* :static)
 (define-cvar music-last-finished-tick::Uint32 :static)
 (define-cvar playing-sound-contexts::GrvSoundContext** :static)
 (define-cvar global-lock::SDL_SpinLock :static)

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
         repl-thread SCM_FALSE
         repl-channel SCM_FALSE
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
         playing-music-context NULL
         playing-music-lock 0)
   (Mix_HookMusicFinished finish-music)

   (Mix_AllocateChannels CHANNEL_SIZE)
   (set! playing-sound-contexts (SCM_NEW_ARRAY (.type GrvSoundContext*) CHANNEL_SIZE))
   (Mix_ChannelFinished finish-sound)

   (set! global-lock 0)
   ) ;; end of initialize-libs

 (initcode
  (initialize-libs))
 ) ;; end of inline-stub

(define-cproc current-ticks ()
  ::<int>
  (return (SDL_GetTicks)))


;;;
;;; Lock
;;;

(inline-stub
 (define-cfn lock-global-var ()
   ::void
   (SDL_AtomicLock (& global-lock)))

 (define-cfn unlock-global-var ()
   ::void
   (SDL_AtomicUnlock (& global-lock)))
 ) ;; end of inline-stub

;;;
;;; Scheduler
;;;

(define *scheduler-thread* #f)
(define *scheduler-channel* #f)

(define (current-timeofday)
  (receive (sec usec) (sys-gettimeofday)
    (+ sec (/. usec 1000000))))

(define (run-scheduler)
  (let1 channel (make-mtqueue)
    (set! *scheduler-thread*
          (make-thread
            (lambda ()
              (guard (e (else (notify-exception e)))
                (define (handle-event event schedules)
                  (match event
                    ('exit
                      #f)
                    (#f
                     (next-schedule schedules))
                    ((sec . thunk)
                     (next-schedule (sort (acons sec thunk schedules)
                                          (lambda (s0 s1)
                                            (< (car s0) (car s1))))))))
                (define (next-schedule schedules)
                  (let1 now-sec (current-timeofday)
                    (match schedules
                      (()
                       (handle-event (dequeue/wait! channel) '()))
                      (((next-sec . thunk) rest ...)
                       (cond
                         ((<= next-sec now-sec)
                          (guard (e (else (notify-exception e)))
                            (thunk))
                          (next-schedule rest))
                         (else
                          (handle-event (dequeue/wait! channel (- next-sec now-sec)) schedules)))))))
                (next-schedule '())))))
    (set! *scheduler-channel* channel)
    (thread-start! *scheduler-thread*)))

(define (shutdown-scheduler)
  (when *scheduler-thread*
    (enqueue! *scheduler-channel* 'exit)
    (thread-join! *scheduler-thread*)
    (set! *scheduler-thread* #f)
    (set! *scheduler-channel* #f)))

(define (kill-scheduler)
  (when *scheduler-thread*
    (thread-terminate! *scheduler-thread*)
    (set! *scheduler-thread* #f)
    (set! *scheduler-channel* #f)))

(define (add-timer! sec thunk)
  (enqueue! *scheduler-channel* (cons (+ (current-timeofday) sec) thunk)))


;;;
;;; Future
;;;

(inline-stub
 (define-cfn notify-uncaught-exception (msg)
   ::void
   (let* ((event::SDL_Event))
     (set! (ref event type) graviton-event-type
           (ref event user code) GRAVITON_UNCAUGHT_EXCEPTION_CODE
           (ref event user data1) msg)
     (SDL_PushEvent (& event))))

 (define-cfn finalize-future (z data::void*)
   ::void
   (when (GRV_FUTURE_P z)
     (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR z)))
       (SDL_DestroyCond (-> gfuture cond))
       (SDL_DestroyMutex (-> gfuture lock))
       (when (and (not (-> gfuture consumed?)) (-> gfuture message))
         (notify-uncaught-exception (SCM_MAKE_STR_COPYING (-> gfuture message)))
         (free (-> gfuture message))))))

 (define-cfn make-future ()
   (let* ((gfuture::GrvFuture* (SCM_NEW GrvFuture))
          (obj (MAKE_GRV_FUTURE gfuture)))
     (set! (-> gfuture lock) (SDL_CreateMutex)
           (-> gfuture cond) (SDL_CreateCond)
           (-> gfuture result) SCM_FALSE
           (-> gfuture exception) SCM_FALSE
           (-> gfuture message) NULL
           (-> gfuture continuations) SCM_NIL
           (-> gfuture consumed?) false)
     (Scm_RegisterFinalizer obj finalize-future NULL)
     (return obj)))

 (define-cfn set-future-result! (gfuture::GrvFuture* result report-error?::bool)
   ::void
   (let* ((conts SCM_NIL)
          (err?::bool false))
     (SDL_LockMutex (-> gfuture lock))
     (cond
       ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
        (set! (-> gfuture result) result
              conts (-> gfuture continuations)
              (-> gfuture continuations) SCM_NIL)
        (SDL_CondSignal (-> gfuture cond)))
       (else
        (set! err? true)))
     (SDL_UnlockMutex (-> gfuture lock))
     (cond
       ((not err?)
        (unless (SCM_NULLP conts)
          (SDL_LockMutex (-> gfuture lock))
          (set! (-> gfuture consumed?) true)
          (SDL_UnlockMutex (-> gfuture lock))
          (cond
            ((Scm_VM)
             (for-each (lambda (cont)
                         (Scm_ApplyRec cont (SCM_LIST2 result SCM_FALSE)))
                       conts))
            (else
             (let* ((event::SDL_Event))
               (set! (ref event type) graviton-event-type
                     (ref event user code) GRAVITON_APPLY_CODE
                     (ref event user data1) conts
                     (ref event user data2) (SCM_LIST2 result SCM_FALSE))
               (SDL_PushEvent (& event)))))))
       (report-error?
        (Scm_Error "result has been already set in <graviton-future>")))))

 (.define MAX_MESSAGE_LENGTH 1024)
 ) ;; end of inline-stub

(define-cproc make-future ()
  (return (make-future)))

(define-cproc set-future-result! (gfuture::<graviton-future> result)
  ::<void>
  (set-future-result! gfuture result true))

(define-cproc set-future-exception! (gfuture::<graviton-future> exception error-message::<const-cstring>)
  ::<void>
  (let* ((conts SCM_NIL)
         (err?::bool false))
    (SDL_LockMutex (-> gfuture lock))
    (cond
      ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
       (let* ((msg::char* (malloc MAX_MESSAGE_LENGTH)))
         (bzero msg MAX_MESSAGE_LENGTH)
         (strncpy msg error-message (- MAX_MESSAGE_LENGTH 1))
         (set! (-> gfuture exception) exception
               (-> gfuture message) msg
               conts (-> gfuture continuations)
               (-> gfuture continuations) SCM_NIL)
         (SDL_CondSignal (-> gfuture cond))))
      (else
       (set! err? true)))
    (SDL_UnlockMutex (-> gfuture lock))
    (when err?
      (Scm_Error "result has been already set in <graviton-future>"))
    (unless (SCM_NULLP conts)
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (Scm_ApplyRec cont (SCM_LIST2 SCM_FALSE exception)))
                conts))))

(define-cproc future-result&exception (gfuture::<graviton-future>)
  ::(<top> <top>)
  (let* ((result SCM_FALSE)
         (exception SCM_FALSE))
    (SDL_LockMutex (-> gfuture lock))
    (loop
     (set! result (-> gfuture result)
           exception (-> gfuture exception))
     (cond
       ((and (SCM_FALSEP result) (SCM_FALSEP exception))
        (SDL_CondWait (-> gfuture cond) (-> gfuture lock)))
       (else
        (break))))
    (set! (-> gfuture consumed?) true)
    (SDL_UnlockMutex (-> gfuture lock))
    (return result exception)))

(define-cproc add-future-continuation! (gfuture::<graviton-future> cont)
  ::<void>
  (let* ((result SCM_FALSE)
         (exception SCM_FALSE)
         (conts SCM_NIL))
    (SDL_LockMutex (-> gfuture lock))
    (set! conts (Scm_Cons cont (-> gfuture continuations))
          (-> gfuture continuations) conts
          result (-> gfuture result)
          exception (-> gfuture exception))
    (SDL_UnlockMutex (-> gfuture lock))
    (unless (and (SCM_FALSEP result) (SCM_FALSEP exception))
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true
            (-> gfuture continuations) SCM_NIL)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (Scm_ApplyRec cont (SCM_LIST2 result exception)))
                conts))))

(define-cproc on-main-thread? ()
  ::<boolean>
  (return (== main-thread-id (SDL_ThreadID))))

(define-cproc submit/main (thunk::<procedure>)
  ::<void>
  (let* ((event::SDL_Event))
    (set! (ref event type) graviton-event-type
          (ref event user code) GRAVITON_APPLY_CODE
          (ref event user data1) (SCM_LIST1 (SCM_OBJ thunk))
          (ref event user data2) SCM_NIL)
    (SDL_PushEvent (& event))))

(define (submit/thread thunk)
  (thread-start! (make-thread
                   (lambda ()
                     (thunk)))))

(define (submit thunk type)
  (let1 %submit (case type
                  ((main) submit/main)
                  ((thread) submit/thread)
                  (else
                   (errorf "type must be 'main or 'thread, but got ~s" thread)))
    (%submit thunk)))

(define (force-future future)
  (receive (result exception) (future-result&exception future)
    (cond
      (result
       (apply values result))
      (exception
       (raise exception))
      (else
       (errorf "[BUG] result or exception must have a value.")))))

(define (%async-apply type proc args)
  (let ((args (apply list args))
        (future (make-future)))
    (submit (lambda ()
              (reset
                (guard (e (else (set-future-exception! future e (report-error e #f))))
                  (receive result (apply proc args)
                    (set-future-result! future result)))))
            type)
    future))

(define (async-apply proc :rest args)
  (%async-apply 'main proc args))

(define (async/thread-apply proc :rest args)
  (%async-apply 'thread proc args))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...)))))

(define-syntax async/thread
  (syntax-rules ()
    ((_ expr ...)
     (async/thread-apply (lambda () expr ...)))))

(define (await future)
  (unless (on-main-thread?)
    (error "await is unavailable on non-main thread."))
  (cond
    ((is-a? future <graviton-future>)
     (shift cont
       (add-future-continuation! future
                                 (lambda (result exception)
                                   (let1 thunk (cond
                                                 (result
                                                  (lambda ()
                                                    (apply cont result)))
                                                 (exception
                                                  (lambda ()
                                                    (raise exception)))
                                                 (else
                                                  (errorf "[BUG] result and exception aren't specified.")))
                                     (submit/main thunk))))))
    (else
     future)))

(define (await-sleep sec)
  (unless (on-main-thread?)
    (error "await-sleep is unavailable on non-main thread."))
  (shift cont
    (add-timer! sec (lambda ()
                      (submit/main cont)))))

(define (report-uncaught-future-exceptions messages)
  (for-each (lambda (msg)
              (display msg (current-error-port))
              (newline (current-error-port)))
            messages)
  (error "async failed, but the exception wasn't caught."))

(define (yield)
  (unless (on-main-thread?)
    (error "yield is unavailable on non-main thread."))
  (shift cont (submit/main cont)))

;;;
;;; Coordinate calculation
;;;

(inline-stub
  (define-cfn convert-coordinate (param::TransformParam* x::double y::double ox::int* oy::int*)
    ::void
    (let* ((m00::double (-> param m00))
           (m10::double (-> param m10))
           (m01::double (-> param m01))
           (m11::double (-> param m11))
           (x0::double (-> param x0))
           (y0::double (-> param y0)))
      (set! (* ox) (cast int (round (+ (* m00 x) (* m01 y) x0)))
            (* oy) (cast int (round (+ (* m10 x) (* m11 y) y0))))))

  (define-cfn compute-transform-param (param::TransformParam*
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
    ::TransformParam*
    (return (& (-> gimage param))))
  ) ;; end of inline-stub

;;;
;;; Image and Texture
;;;

(inline-stub
  (define-cfn finalize-image (z data::void*)
    ::void
    (when (GRV_IMAGE_P z)
      (let* ((gimage::GrvImage* (GRV_IMAGE_PTR z)))
        (unless (SCM_NULLP (-> gimage texture-alist))
          (fprintf stderr "[BUG] texture_alist in <graviton-image> must be nil in finalizer. forgot to call release-texture?\n"))
        (SDL_FreeSurface (-> gimage surface))
        (set! (-> gimage surface) NULL
              (-> gimage texture-alist) SCM_NIL))))

  (define-cfn update-rect (gimage::GrvImage* x::int y::int w::int h::int)
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

  (define-cfn retain-texture (win gimage::GrvImage*)
    ::void
    (unless (GRV_WINDOW_P win)
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
                 texture (MAKE_GRV_TEXTURE gtexture)
                 (-> gimage texture-alist) (Scm_Cons (Scm_Cons win texture) (-> gimage texture-alist) ))))
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
              (-> gimage texture-alist))
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
                (-> gimage texture-alist))
      (unless (GRV_TEXTURE_P texture)
        (Scm_Error "<graviton-texture> not found for %S (got %S), forgot retain-texture?" win texture))

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

  (define-cfn decompose-rgba (color::Uint32 r::Uint8* g::Uint8* b::Uint8* a::Uint8*)
    ::void
    (cond
      ((== SDL_BYTEORDER SDL_LIL_ENDIAN)
       (set! (* r) (logand color #xff)
             (* g) (logand (>> color 8) #xff)
             (* b) (logand (>> color 16) #xff)
             (* a) (logand (>> color 24) #xff)))
      (else
       (set! (* r) (logand (>> color 24) #xff)
             (* g) (logand (>> color 16) #xff)
             (* b) (logand (>> color 8) #xff)
             (* a) (logand color #xff)))))

  (define-cfn bitblt (src-gimage::GrvImage* src-rect::SDL_Rect* dst-gimage::GrvImage* dst-rect::SDL_Rect* color::Uint32)
    ::void
    (let* ((r::Uint8)
           (g::Uint8)
           (b::Uint8)
           (a::Uint8))
      (decompose-rgba color (& r) (& g) (& b) (& a))
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
      (update-rect dst-gimage (-> dst-rect x) (-> dst-rect y) (-> dst-rect w) (-> dst-rect h))))

  ) ;; end of inline-stub

(define-cproc make-image (w::<int> h::<int>)
  (let* ((surface::SDL_Surface* (SDL_CreateRGBSurfaceWithFormat 0 w h 32 SDL_PIXELFORMAT_RGBA32))
         (gimage::GrvImage* (SCM_NEW (.type GrvImage)))
         (obj (MAKE_GRV_IMAGE gimage)))
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

(define (save-image image filename :key (format 'png))
  (unless (eq? format 'png)
    (errorf "Unsupported format: ~s" format))
  (call-with-output-file filename
    (lambda (out)
      (let ((width (image-width image))
            (height (image-height image))
            (image-buffer (uvector-alias <u8vector> (image-rgba-pixels image))))
        (write-png-image width height image-buffer out)))))

(define-cproc set-image-border! (gimage::<graviton-image> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (compute-transform-param (& (-> gimage param)) (-> gimage surface w) (-> gimage surface h) left top right bottom false))

(define-cproc image-size (image)
  ::<list>
  (cond
    ((GRV_IMAGE_P image)
     (let* ((gimage::GrvImage* (GRV_IMAGE_PTR image)))
       (return (SCM_LIST2 (SCM_MAKE_INT (-> gimage surface w)) (SCM_MAKE_INT (-> gimage surface h))))))
    ((GRV_TILE_IMAGE_P image)
     (let* ((gtile::GrvTileImage* (GRV_TILE_IMAGE_PTR image)))
       (return (SCM_LIST2 (SCM_MAKE_INT (ref (-> gtile rect) w)) (SCM_MAKE_INT (ref (-> gtile rect) h))))))
    (else
     (Scm_Error "<graviton-image> or <graviton-tile-image> required, but got %S" image))))

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

(define-cproc image-rgba (image::<graviton-image> x::<double> y::<double>)
  ::<int32>
  (let* ((ox::int)
         (oy::int))
    (image-coordinate image x y (& ox) (& oy))
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

(define (image? obj)
  (is-a? obj <graviton-image>))

(define-cproc bitblt (src-image dst-gimage::<graviton-image> dst-position::<list>
                                :key (src-position #f) (src-size #f) (dst-size #f) (color::<uint32> #xffffffff))
  ::<void>
  (let* ((src-gimage::GrvImage*)
         (src-rect::SDL_Rect*)
         (dst-rect::SDL_Rect))
    (cond
      ((GRV_TILE_IMAGE_P src-image)
       (unless (SCM_FALSEP src-position)
         (Scm_Error "src-position can't be specified when src-image is <graviton-tile-image>"))
       (unless (SCM_FALSEP src-size)
         (Scm_Error "src-size can't be specified when src-image is <graviton-tile-image>"))
       (set! src-gimage (GRV_IMAGE_PTR (-> (GRV_TILE_IMAGE_PTR src-image) image))
             src-rect (& (-> (GRV_TILE_IMAGE_PTR src-image) rect))))
      ((GRV_IMAGE_P src-image)
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
          (image-coordinate src-gimage
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
    (image-coordinate dst-gimage
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
    (bitblt src-gimage src-rect dst-gimage (& dst-rect) color)))


;;;
;;; TileImage
;;;

(define-cproc make-tile-image (image x::<int> y::<int> w::<int> h::<int>)
  ::<graviton-tile-image>
  (let* ((gtile::GrvTileImage* (SCM_NEW (.type GrvTileImage))))
    (unless (GRV_IMAGE_P image)
      (Scm_Error "<graviton-image> required, but got %S" image))
    (set! (-> gtile image) image
          (ref (-> gtile rect) x) x
          (ref (-> gtile rect) y) y
          (ref (-> gtile rect) w) w
          (ref (-> gtile rect) h) h)
    (return gtile)))

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

  (define-cfn render-sprite (gsprite::GrvSprite*)
    ::void
    (when (or (SCM_FALSEP (-> gsprite image))
              (not (-> gsprite visible)))
      (return))
    (let* ((gimage::GrvImage* (GRV_IMAGE_PTR (-> gsprite image))))
      (refresh-textures gimage)
      (let* ((angle-deg::double)
             (zoom-x::double)
             (zoom-y::double)
             (flip::SDL_RendererFlip))
        (compute-sprite-actual-params gsprite (& angle-deg) (& zoom-x) (& zoom-y) (& flip))
        (let* ((win (-> gsprite window))
               (gwin::GrvWindow* (GRV_WINDOW_PTR win))
               (texture::SDL_Texture* (get-texture win gimage))
               (spr-w::double (* (-> gsprite srcrect w) zoom-x))
               (spr-h::double (* (-> gsprite srcrect h) zoom-y))
               (dstrect::SDL_Rect)
               (r::Uint8)
               (g::Uint8)
               (b::Uint8)
               (a::Uint8))
          (decompose-rgba (-> gsprite color) (& r) (& g) (& b) (& a))
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
        (retain-texture window (GRV_IMAGE_PTR sprite-image)))

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
      (let* ((sprite (MAKE_GRV_SPRITE gsprite)))
        (Scm_RegisterFinalizer sprite finalize-sprite NULL)
        (insert-window-sprite sprite)
        (return sprite))))
  ) ;; end of inline-stub

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
  (unless (GRV_WINDOW_P window)
    (Scm_Error "window must be <graviton-window>, but got %S" window))
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image)
              (GRV_TILE_IMAGE_P image))
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
      ((GRV_IMAGE_P image)
       (set! sprite-image image
             srcrect (SCM_NEW SDL_Rect)
             (-> srcrect x) 0
             (-> srcrect y) 0
             (-> srcrect w) (-> (GRV_IMAGE_PTR image) surface w)
             (-> srcrect h) (-> (GRV_IMAGE_PTR image) surface h)))
      ((GRV_TILE_IMAGE_P image)
       (set! sprite-image (-> (GRV_TILE_IMAGE_PTR image) image)
             srcrect (& (-> (GRV_TILE_IMAGE_PTR image) rect)))))

    (return (make-sprite window sprite-image x y z srcrect angle zoom-x zoom-y visible? color))))

(define-cproc set-sprite-image! (gsprite::<graviton-sprite> image)
  ::<void>
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image)
              (GRV_TILE_IMAGE_P image))
    (Scm_Error "image must be <graviton-image>, <graviton-tile-image> or #f, but got %S" image))

  (let* ((sprite-image SCM_FALSE)
         (srcrect::SDL_Rect* NULL))
    (cond
      ((GRV_IMAGE_P image)
       (let* ((gimage::GrvImage* (GRV_IMAGE_PTR image)))
         (set! sprite-image image
               srcrect (SCM_NEW SDL_Rect)
               (-> srcrect x) 0
               (-> srcrect y) 0
               (-> srcrect w) (-> gimage surface w)
               (-> srcrect h) (-> gimage surface h))))
      ((GRV_TILE_IMAGE_P image)
       (set! sprite-image (-> (GRV_TILE_IMAGE_PTR image) image)
             srcrect (& (-> (GRV_TILE_IMAGE_PTR image) rect)))))

    (unless (SCM_FALSEP sprite-image)
      (retain-texture (-> gsprite window) (GRV_IMAGE_PTR sprite-image)))
    (unless (SCM_FALSEP (-> gsprite image))
      (release-texture (-> gsprite window) (GRV_IMAGE_PTR (-> gsprite image))))

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
  (unless (GRV_SPRITE_P sprite)
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


;;;
;;; TileMap
;;;

(inline-stub
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
            (unless (GRV_TILE_IMAGE_P tile)
              (Scm_Error "<graviton-tile-image> required, but got %S" tile))
            (set! gtile (GRV_TILE_IMAGE_PTR tile))
            (bitblt (GRV_IMAGE_PTR (-> gtile image))
                    (& (-> gtile rect))
                    gimage
                    (& dstrect)
                    fg-color))))))
  ) ;; end of inline-stub

(define-cproc make-tile-map (window tile-images::<vector> columns::<int> rows::<int> x::<double> y::<double> :key (z::<double> 0) (fill::<uint32> 0))
  ::<graviton-tile-map>
  (let* ((gtilemap::GrvTileMap* (SCM_NEW GrvTileMap))
         (size::int (* columns rows))
         (tile-width::int)
         (tile-height::int)
         (sprite))
    (when (== (SCM_VECTOR_SIZE tile-images) 0)
      (Scm_Error "tile-images must have at least one element"))
    (dotimes (i (SCM_VECTOR_SIZE tile-images))
      (unless (GRV_TILE_IMAGE_P (Scm_VectorRef tile-images i SCM_UNBOUND))
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

    (set! (-> gtilemap columns) columns
          (-> gtilemap rows) rows
          (-> gtilemap offset) 0

          (-> gtilemap image) (Scm_EvalRec (SCM_LIST3 'make-image
                                                      (SCM_MAKE_INT (* tile-width columns))
                                                      (SCM_MAKE_INT (* tile-height rows)))
                                           graviton-module)
          (-> gtilemap tile-images) (SCM_OBJ tile-images)
          (-> gtilemap tile-width) tile-width
          (-> gtilemap tile-height) tile-height

          sprite (Scm_EvalRec (Scm_List 'make-sprite
                                        window
                                        ':image (-> gtilemap image)
                                        ':x (Scm_MakeFlonum x)
                                        ':y (Scm_MakeFlonum y)
                                        ':z (Scm_MakeFlonum z)
                                        NULL)
                              graviton-module))
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


;; Virtual Text Driver
(define (code4->color n)
  (case n
    ((30) (rgb 0 0 0))
    ((40) (rgba 0 0 0 0))
    ((31 41) (rgb 170 0 0))
    ((32 42) (rgb 0 170 0))
    ((33 43) (rgb 170 85 0))
    ((34 44) (rgb 0 0 170))
    ((35 45) (rgb 170 0 170))
    ((36 46) (rgb 0 170 170))
    ((37 47) (rgb 170 170 170))
    ((90 100) (rgb 85 85 85))
    ((91 101) (rgb 255 85 85))
    ((92 102) (rgb 85 255 85))
    ((93 103) (rgb 255 255 85))
    ((94 104) (rgb 85 85 255))
    ((95 105) (rgb 255 85 255))
    ((96 106) (rgb 85 255 255))
    ((97 107) (rgb 255 255 255))
    (else
     (rgb 255 255 255))))

(define (code8->color n fg-color?)
  (cond
    ((<= 0 n 7)
     (code4->color (+ (if fg-color? 30 40) n)))
    ((<= 8 n 15)
     (code4->color (+ (if fg-color? 90 100) (- n 8))))
    ((<= 16 n 231)
     (let* ((v (- n 16))
            (d (/ 255 5))
            (b (* d (modulo v 6)))
            (g (* d (modulo (quotient v 6) 6)))
            (r (* d (quotient v 36))))
       (rgb r g b)))
    ((<= 232 n 255)
     (let1 v (round->exact (* (/ 255 23) (- n 232)))
       (rgb v v v)))
    (else
     (rgb 255 255 255))))

(define (make-text-driver tile-map)
  (let ((cursor-x 0)
        (cursor-y 0)
        (fg-color #xffffffff)
        (bg-color 0)
        (state 'normal)
        (escape-sequence '())
        (saved-cursor-position '())
        (esc-charset (list->char-set '(#\[ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\;))))
    (define (forward-cursor)
      (inc! cursor-x)
      (when (<= (tile-map-columns tile-map) cursor-x)
        (set! cursor-x 0)
        (inc! cursor-y))
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (backword-cursor)
      (dec! cursor-x)
      (when (< cursor-x 0)
        (set! cursor-x (- (tile-map-columns tile-map) 1))
        (dec! cursor-y))
      (when (< cursor-y 0)
        (set! cursor-x 0)
        (set! cursor-y 0)))
    (define (up-cursor)
      (dec! cursor-y)
      (when (< cursor-y 0)
        (scroll-down)
        (set! cursor-y 0)))
    (define (down-cursor)
      (inc! cursor-y)
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (htab n)
      (set! cursor-x (* (+ (quotient cursor-x n) 1) n))
      (when (<= (tile-map-columns tile-map) cursor-x)
        (set! cursor-x 0)
        (inc! cursor-y))
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (lf)
      (set! cursor-x 0)
      (inc! cursor-y)
      (when (<= (tile-map-rows tile-map) cursor-y)
        (scroll-up)
        (set! cursor-y (- (tile-map-rows tile-map) 1))))
    (define (cr)
      (set! cursor-x 0))
    (define (delete-char)
      (set-tile-map-tile! tile-map cursor-x cursor-y 0))
    (define (clear-line y)
      (dotimes (x (tile-map-columns tile-map))
        (set-tile-map-tile! tile-map x y 0)))
    (define (clear-screen)
      (dotimes (y (tile-map-rows tile-map))
        (clear-line y)))
    (define (scroll-up)
      (inc! (tile-map-offset tile-map) (tile-map-columns tile-map))
      (clear-line (- (tile-map-rows tile-map) 1)))
    (define (scroll-down)
      (dec! (tile-map-offset tile-map) (tile-map-columns tile-map))
      (clear-line 0))
    (define (erase-in-display n)
      (case n
        ((0) ;; clear from cursor to end of screen.
         (erase-in-line 0)
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y (+ cursor-y 1) (tile-map-rows tile-map))
                (set-tile-map-tile! x y 0)))
        ((1) ;; clear from cursor to beginning of the screen.
         (erase-in-line 1)
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y 0 cursor-y)
                (set-tile-map-tile! x y 0)))
        ((2 3) ;; clear entire screen
         (do-ec (: x 0 (tile-map-columns tile-map))
                (: y 0 (tile-map-rows tile-map))
                (set-tile-map-tile! x y 0)))))
    (define (erase-in-line n)
      (case n
        ((0) ;; clear from cursor to end of line.
         (do-ec (: x cursor-x (tile-map-columns tile-map))
                (set-tile-map-tile! x cursor-y 0)))
        ((1) ;; clear from cursor to beginning of the line.
         (do-ec (: x 0 (+ cursor-x 1))
                (set-tile-map-tile! x cursor-y 0)))
        ((2) ;; clear entire line.
         (do-ec (: x 0 (tile-map-columns tile-map))
                (set-tile-map-tile! x cursor-y 0)))))
    (define (escape-sequence->string)
      (list->string (reverse escape-sequence)))
    (define (driver c)
      (case state
        ((normal)
         (let1 code (char->ucs c)
           (cond
             ((= code #x08)
              (backword-cursor)
              (delete-char))
             ((= code #x09)
              (htab 8))
             ((= code #x0a)
              (lf))
             ((= code #x0d)
              (cr))
             ((= code #x1b)
              (set! state 'escape))
             ((and (<= 0 code) (< code (vector-length (tile-map-tile-images tile-map))))
              (set-tile-map-tile! tile-map cursor-x cursor-y code :foreground-color fg-color :background-color bg-color)
              (forward-cursor)))))
        ((escape)
         (cond
           ((eqv? c #\[)
            (set! escape-sequence '())
            (set! state 'escape-sequence))
           (else
            (set! state 'normal))))
        ((escape-sequence)
         (cond
           ((char-set-contains? esc-charset c)
            (push! escape-sequence c))
           ((eqv? c #\A)
            ;; Cursor up
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (up-cursor)))
            (set! state 'normal))
           ((eqv? c #\B)
            ;; Cursor down
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (down-cursor)))
            (set! state 'normal))
           ((eqv? c #\C)
            ;; Cursor forward
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (forward-cursor)))
            (set! state 'normal))
           ((eqv? c #\D)
            ;; Cursor back
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (dotimes (_ n)
                (backward-cursor)))
            (set! state 'normal))
           ((eqv? c #\E)
            ;; Cursor next line
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x 0)
              (dotimes (_ n)
                (down-cursor)))
            (set! state 'normal))
           ((eqv? c #\F)
            ;; Cursor previous line
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x 0)
              (dotimes (_ n)
                (up-cursor)))
            (set! state 'normal))
           ((eqv? c #\G)
            ;; Cursor horizontal absolute
            (let1 n (or (string->number (escape-sequence->string)) 1)
              (set! cursor-x (min (- n 1) (- (tile-map-columns tile-map) 1))))
            (set! state 'normal))
           ((or (eqv? c #\H) (eqv? c #\f))
            ;; Cursor move position
            (rxmatch-case (escape-sequence->string)
              (#/(\d*)\;(\d*)/ (_ n m)
               (let ((row (min (or (string->number n) 1) (tile-map-rows tile-map)))
                     (col (min (or (string->number m) 1) (tile-map-columns tile-map))))
                 (set! cursor-x (- col 1))
                 (set! cursor-y (- row 1)))))
            (set! state 'normal))
           ((eqv? c #\J)
            ;; Erase in display
            (erase-in-display (or (string->number (escape-sequence->string)) 1))
            (set! state 'normal))
           ((eqv? c #\K)
            ;; Erase in line
            (erase-in-line (or (string->number (escape-sequence->string)) 1))
            (set! state 'normal))
           ((eqv? c #\S)
            ;; Scroll up
            (dotimes (_ (or (string->number (escape-sequence->string)) 1))
              (scroll-up))
            (set! state 'normal))
           ((eqv? c #\T)
            ;; Scroll down
            (dotimes (_ (or (string->number (escape-sequence->string)) 1))
              (scroll-down))
            (set! state 'normal))
           ((eqv? c #\s)
            ;; Save cursor position
            (push! saved-cursor-position (cons cursor-x cursor-y))
            (set! state 'normal))
           ((eqv? c #\u)
            ;; Restore cursor position
            (unless (null? saved-cursor-position)
              (match (pop! saved-cursor-position)
                ((x . y)
                 (set! cursor-x x)
                 (set! cursor-y y))))
            (set! state 'normal))
           ((eqv? c #\m)
            (rxmatch-case (escape-sequence->string)
              (#/^0?$/ (_)
               (set! fg-color #xffffffff)
               (set! bg-color 0))
              (#/^1\;3(\d)$/ (_ v)
               (set! fg-color (+ 90 (string->number v))))
              (#/^1\;4(\d)$/ (_ v)
               (set! fg-color (+ 100 (string->number v))))
              (#/^(\d+)$/ (_ v)
               (let1 v (or (string->number v) 0)
                 (cond
                   ((or (<= 30 v 37) (<= 90 v 97))
                    (set! fg-color (code4->color v)))
                   ((or (<= 40 v 47) (<= 100 v 107))
                    (set! bg-color (code4->color v))))))
              (#/^(\d+)\;(\d+)$/ (_ v0 v1)
               (for-each (lambda (v)
                           (let1 v (or (string->number v) 0)
                             (cond
                               ((or (<= 30 v 37) (<= 90 v 97))
                                (set! fg-color (code4->color v)))
                               ((or (<= 40 v 47) (<= 100 v 107))
                                (set! bg-color (code4->color v))))))
                         (list v0 v1)))
              (#/^38\;5\;(\d+)$/ (_ n)
               (set! fg-color (code8->color (or (string->number n) 0) #t)))
              (#/^48\;5\;(\d+)$/ (_ n)
               (set! bg-color (code8->color (or (string->number n) 0) #f)))
              (#/^38\;2\;(\d+)\;(\d+)\;(\d+)$/ (_ r g b)
               (set! fg-color (rgb (or (string->number r) 0)
                                   (or (string->number g) 0)
                                   (or (string->number b) 0))))
              (#/^48\;2\;(\d+)\;(\d+)\;(\d+)$/ (_ r g b)
               (set! bg-color (rgb (or (string->number r) 0)
                                   (or (string->number g) 0)
                                   (or (string->number b) 0)))))
            (set! state 'normal))
               ) ;; end of cond
              )  ;; end of escape-sequence in case
            )    ;; end of case
           )     ;; end of define
    driver))

(define (tile-map->output-port tile-map)
  (let ((out (make <virtual-output-port>))
        (driver (make-text-driver tile-map)))
    (slot-set! out 'putc driver)
    (slot-set! out 'putb (lambda (b)
                           (driver (ucs->char b))))
    out))


;;;
;;; Window
;;;

(inline-stub
  (define-cfn remove-destroyed-windows ()
    ::void
    (let* ((prev SCM_NIL)
           (wins SCM_NIL))
      (for-each (lambda (win)
                  (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
                    (when (-> gwin window)
                      (set! wins (Scm_Cons win wins)))))
                grv-windows)
      (set! grv-windows wins)))

  (define-cfn destroy-window (gwin::GrvWindow*)
    ::void
    (for-each (lambda (sprite)
                (invalidate-sprite (GRV_SPRITE_PTR sprite)))
              (-> gwin sprites))
    (set! (-> gwin sprites) SCM_NIL)

    (SDL_DestroyRenderer (-> gwin renderer))
    (SDL_DestroyWindow (-> gwin window))
    (set! (-> gwin window) NULL
          (-> gwin renderer) NULL))

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

  (define-cfn set-window-logical-size! (gwin::GrvWindow* physical-width::int physical-height::int logical-width::int logical-height::int)
    ::void
    (let* ((zoom-w::double)
           (zoom-h::double)
           (zoom::double)
           (renderer-logical-width::int)
           (renderer-logical-height::int))
      (set! zoom-w (/ (cast double physical-width) (cast double logical-width))
            zoom-h (/ (cast double physical-height) (cast double logical-height))
            zoom (?: (< zoom-w zoom-h) zoom-w zoom-h)
            (-> gwin logical-width) logical-width
            (-> gwin logical-height) logical-height
            renderer-logical-width (cast int (round (/ physical-width zoom)))
            renderer-logical-height (cast int (round (/ physical-height zoom)))
            (-> gwin offset-x) (cast int (round (/ (- renderer-logical-width logical-width) 2.0)))
            (-> gwin offset-y) (cast int (round (/ (- renderer-logical-height logical-height) 2.0)))
            (-> gwin clip) (SCM_NEW SDL_Rect)
            (-> gwin clip w) logical-width
            (-> gwin clip h) logical-height
            (-> gwin clip x) (-> gwin offset-x)
            (-> gwin clip y) (-> gwin offset-y))
      (when (< (SDL_RenderSetLogicalSize (-> gwin renderer) renderer-logical-width renderer-logical-height) 0)
        (Scm_Error "SDL_RenderSetLogicalSize failed: %s" (SDL_GetError)))))
  ) ;; end of inline-stub

(define-cproc make-window (title::<const-cstring> logical-width::<int> logical-height::<int> :key (resizable?::<boolean> #f) (icon #f) (shown?::<boolean> #t) (maximized?::<boolean> #f) (minimized?::<boolean> #f) (fullscreen?::<boolean> #f))
  (let* ((flags::Uint32 0))
    (when fullscreen?
      (set! flags (logior flags SDL_WINDOW_FULLSCREEN_DESKTOP)))
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
            (-> gwin sprites) SCM_NIL
            (-> gwin icon) icon
            (-> gwin handler-table) (Scm_MakeHashTableSimple SCM_HASH_EQ 16))

      (set! (-> gwin window) (SDL_CreateWindow title
                                               SDL_WINDOWPOS_UNDEFINED
                                               SDL_WINDOWPOS_UNDEFINED
                                               logical-width
                                               logical-height
                                               flags))
      (unless (-> gwin window)
        (Scm_Error "SDL_CreateWindow failed: %s" (SDL_GetError)))

      (when (GRV_IMAGE_P icon)
        (SDL_SetWindowIcon (-> gwin window) (-> (GRV_IMAGE_PTR icon) surface)))

      (set! (-> gwin renderer) (SDL_CreateRenderer (-> gwin window) -1 0))
      (unless (-> gwin renderer)
        (Scm_Error "SDL_CreateRenderer failed: %s" (SDL_GetError)))

      (let* ((physical-width::int)
             (physical-height::int))
        (SDL_GetWindowSize (-> gwin window) (& physical-width) (& physical-height))
        (set-window-logical-size! gwin physical-width physical-height logical-width logical-height))

      (Scm_HashTableSet (SCM_HASH_TABLE (-> gwin handler-table))
                        'window-close
                        (Scm_EvalRec 'destroy-window
                                     (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0)))
                        0)
      (Scm_HashTableSet (SCM_HASH_TABLE (-> gwin handler-table))
                        'window-resized
                        (Scm_EvalRec 'reflect-resized-window-parameter
                                     (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0)))
                        0)

      (let* ((win (MAKE_GRV_WINDOW gwin)))
        (set! grv-windows (Scm_Cons win grv-windows))
        (return win)))))

(define-cproc clear-window-sprites! (gwin::<graviton-window>)
  ::<void>
  (for-each (lambda (sprite)
              (let* ((gsprite::GrvSprite* (GRV_SPRITE_PTR gsprite)))
                (invalidate-sprite gsprite)))
            (-> gwin sprites))
  (set! (-> gwin sprites) SCM_NIL))

(define-cproc destroy-window (gwin::<graviton-window>)
  ::<void>
  (destroy-window gwin)
  (remove-destroyed-windows))

(define-cproc destroy-all-windows ()
  ::<void>
  (for-each (lambda (obj)
              (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj)))
                (destroy-window gwin)))
            grv-windows)
  (remove-destroyed-windows))

(define-cproc send-close-window-event (gwin::<graviton-window>)
  ::<void>
  (unless (-> gwin window)
    (return))
  (let* ((event::SDL_Event))
    (set! (ref event window type) SDL_WINDOWEVENT
          (ref event window windowID) (SDL_GetWindowID (-> gwin window))
          (ref event window event) SDL_WINDOWEVENT_CLOSE)
    (SDL_PushEvent (& event))))

(define-cproc window-physical-size (gwin::<graviton-window>)
  ::(<int> <int>)
  (let* ((w::int)
         (h::int))
    (SDL_GetWindowSize (-> gwin window) (& w) (& h))
    (return w h)))

(define (window-physical-width window)
  (values-ref (window-physical-size window) 0))

(define (window-physical-height window)
  (values-ref (window-physical-size window) 1))

(define-cproc set-window-physical-size! (gwin::<graviton-window> physical-width::<int> physical-height::<int>)
  ::<void>
  (SDL_SetWindowSize (-> gwin window) physical-width physical-height)
  (set-window-logical-size! gwin physical-width physical-height (-> gwin logical-width) (-> gwin logical-height)))

(define-cproc window-logical-size (gwin::<graviton-window>)
  ::(<int> <int>)
  (return (-> gwin logical-width) (-> gwin logical-height)))

(define (window-logical-width window)
  (values-ref (window-logical-size window) 0))

(define (window-logical-height window)
  (values-ref (window-logical-size window) 1))

(define-cproc set-window-logical-size! (gwin::<graviton-window> logical-width::<int> logical-height::<int>)
  ::<void>
  (let* ((physical-width::int)
         (physical-height::int))
    (SDL_GetWindowSize (-> gwin window) (& physical-width) (& physical-height))
    (set-window-logical-size! gwin physical-width physical-height logical-width logical-height)))

(define-cproc window-fullscreen? (gwin::<graviton-window>)
  ::<boolean>
  (let* ((flags::Uint32 (SDL_GetWindowFlags (-> gwin window))))
    (return (logand flags (logior SDL_WINDOW_FULLSCREEN_DESKTOP SDL_WINDOW_FULLSCREEN)))))

(define-cproc set-window-fullscreen! (gwin::<graviton-window> flag::<boolean>)
  ::<void>
  (when (< (SDL_SetWindowFullscreen (-> gwin window) (?: flag SDL_WINDOW_FULLSCREEN_DESKTOP 0)) 0)
    (Scm_Error "SDL_SetWindowFullscreen failed: %s" (SDL_GetError))))

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

(define-cproc reflect-resized-window-parameter (gwin::<graviton-window> physical-width::<int> physical-height::<int>)
  ::<void>
  (set-window-logical-size! gwin physical-width physical-height (-> gwin logical-width) (-> gwin logical-height)))

(define-cproc window-handler-table (gwin::<graviton-window>)
  (return (-> gwin handler-table)))

(define (set-window-handler! window event proc)
  (hash-table-set! (window-handler-table window) event proc))

(define-cproc all-windows ()
  (return grv-windows))

(define (last-window)
  (let1 wins (all-windows)
    (cond
      ((null? wins)
       #f)
      (else
       (car wins)))))

(define (window? obj)
  (is-a? obj <graviton-window>))

;;;
;;; REPL
;;;

(inline-stub
  (define-cfn is-repl-running? ()
    ::bool
    (if (and (not (SCM_FALSEP repl-thread))
             (== (-> (SCM_VM repl-thread) state) SCM_VM_RUNNABLE))
        (return true)
        (return false)))
  ) ;; end of inline-stub

(define-cproc repl-thread ()
  (return repl-thread))

(define-cproc repl-channel ()
  (return repl-channel))

(define-cproc set-repl! (thread channel)
  ::<void>
  (set! repl-thread thread)
  (set! repl-channel channel))

(define-cproc is-repl-running? ()
  ::<boolean>
  (return (is-repl-running?)))

(define (is-interactive?)
  (let ((gosh? (and (not (null? (command-line)))
                    (equal? (values-ref (decompose-path (car (command-line))) 1) "gosh"))))
    (and gosh? (find-module 'gauche.interactive))))

(define run-graviton-repl? (make-parameter (is-interactive?)))

(define (%evaluator sexpr _)
  (let ((%set-history-exception! (eval '%set-history-exception! (find-module 'gauche.interactive)))
        (%set-history-expr! (eval '%set-history-expr! (find-module 'gauche.interactive))))
    (guard (e (else (%set-history-exception! e)
                    (raise e)))
      (receive r (force-future (async
                                 (eval sexpr ((with-module gauche.internal vm-current-module)))))
        (%set-history-expr! r)
        (apply values r)))))

(define %prompter
  (let1 user-module (find-module 'user)
    (lambda ()
      (let1 m (force-future (async
                              ((with-module gauche.internal vm-current-module))))
        (if (eq? m user-module)
            (display "graviton> ")
            (format #t "graviton[~a]> " (module-name m)))
        (flush)))))

(define (make-printer)
  (eval '%printer (find-module 'gauche.interactive)))

(define (make-reader channel)
  (lambda ()
    (let1 signal (dequeue! channel #f)
      (match signal
        ('exit
          (eof-object))
        (_
         (match (read)
           (('unquote command)
            (eval `(handle-toplevel-command ',command ',(read-line))
                  (find-module 'gauche.interactive)))
           (expr
            (unless (eof-object? expr)
              (consume-trailing-whitespaces))
            expr)))))))

(define (run-grv-repl in out err)
  (let* ((channel (make-mtqueue))
         (repl-thread (make-thread
                        (lambda ()
                          (with-ports in out err
                            (lambda ()
                              (guard (e (else (report-error e)))
                                (read-eval-print-loop (make-reader channel)
                                                      %evaluator
                                                      (make-printer)
                                                      %prompter))))))))
    (thread-start! repl-thread)
    (set-repl! repl-thread channel)))

(define (exit-repl)
  (when (is-repl-running?)
    (enqueue! (repl-channel) 'exit))
  (undefined))

(define (kill-repl)
  (when (is-repl-running?)
    (unwind-protect
        (guard (e (else (report-error e)))
          (thread-terminate! (repl-thread)))
      (set-repl! #f #f)))
  (undefined))


;;;
;;; Event
;;;

(inline-stub
 (define-cfn %call-window-handler (win event args)
   ::void
   (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
     (let* ((handler (Scm_HashTableRef (SCM_HASH_TABLE (-> gwin handler-table)) event SCM_FALSE)))
       (when (SCM_PROCEDUREP handler)
         (Scm_ApplyRec handler (Scm_Cons win args))
         (return)))))

 (define-cfn call-window-handler (window-id::Uint32 event args)
   ::void
   (for-each (lambda (win)
               (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
                 (when (== (SDL_GetWindowID (-> gwin window)) window-id)
                   (%call-window-handler win event args)
                   (return))))
             grv-windows))

 (define-cfn call-global-handler (event args)
   ::void
   (let* ((handler (Scm_HashTableRef (SCM_HASH_TABLE global-handler-table) event SCM_FALSE)))
     (when (SCM_PROCEDUREP handler)
       (Scm_ApplyRec handler args))))

 (define-cfn process-event (sdl-event::SDL_Event*)
   ::ScmObj
   (case (-> sdl-event type)
     ((SDL_WINDOWEVENT)
      (case (ref (-> sdl-event window) event)
        ((SDL_WINDOWEVENT_MOVED SDL_WINDOWEVENT_RESIZED SDL_WINDOWEVENT_SIZE_CHANGED)
         (call-window-handler (ref (-> sdl-event window) windowID)
                              (window-event->symbol (ref (-> sdl-event window) event))
                              (SCM_LIST2 (SCM_MAKE_INT (ref (-> sdl-event window) data1))
                                         (SCM_MAKE_INT (ref (-> sdl-event window) data2)))))
        (else
         (call-window-handler (ref (-> sdl-event window) windowID)
                              (window-event->symbol (ref (-> sdl-event window) event))
                              SCM_NIL))))
     ((SDL_KEYDOWN SDL_KEYUP)
      (call-window-handler (ref (-> sdl-event key) windowID)
                           (?: (== (-> sdl-event type) SDL_KEYDOWN) 'key-down 'key-up)
                           (SCM_LIST4 (scancode->symbol (ref (-> sdl-event key) keysym scancode))
                                      (keycode->symbol (ref (-> sdl-event key) keysym sym))
                                      (kmod->symbols (ref (-> sdl-event key) keysym mod))
                                      (SCM_MAKE_BOOL (ref (-> sdl-event key) repeat)))))
     ((SDL_TEXTEDITING)
      (call-window-handler (ref (-> sdl-event edit) windowID)
                           'text-editing
                           (SCM_LIST3 (SCM_MAKE_STR_COPYING (ref (-> sdl-event edit) text))
                                      (SCM_MAKE_INT (ref (-> sdl-event edit) start))
                                      (SCM_MAKE_INT (ref (-> sdl-event edit) length)))))
     ((SDL_TEXTINPUT)
      (call-window-handler (ref (-> sdl-event text) windowID)
                           'text-input
                           (SCM_LIST1 (SCM_MAKE_STR_COPYING (ref (-> sdl-event text) text)))))
     ((SDL_MOUSEMOTION)
      (call-window-handler (ref (-> sdl-event motion) windowID)
                           'mouse-motion
                           (Scm_List (SCM_MAKE_INT (ref (-> sdl-event motion) which))
                                     (mouse-button-state->symbols (ref (-> sdl-event motion) state))
                                     (SCM_MAKE_INT (ref (-> sdl-event motion) x))
                                     (SCM_MAKE_INT (ref (-> sdl-event motion) y))
                                     (SCM_MAKE_INT (ref (-> sdl-event motion) xrel))
                                     (SCM_MAKE_INT (ref (-> sdl-event motion) yrel))
                                     NULL)))
     ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
      (call-window-handler (ref (-> sdl-event button) windowID)
                           (?: (== (-> sdl-event type) SDL_MOUSEBUTTONDOWN) 'mouse-button-down 'mouse-button-up)
                           (SCM_LIST5 (SCM_MAKE_INT (ref (-> sdl-event button) which))
                                      (mouse-button->symbol (ref (-> sdl-event button) button))
                                      (SCM_MAKE_INT (ref (-> sdl-event button) clicks))
                                      (SCM_MAKE_INT (ref (-> sdl-event button) x))
                                      (SCM_MAKE_INT (ref (-> sdl-event button) y)))))
     ((SDL_MOUSEWHEEL)
      (call-window-handler (ref (-> sdl-event wheel) windowID)
                           'mouse-wheel
                           (SCM_LIST4 (SCM_MAKE_INT (ref (-> sdl-event wheel) which))
                                      (SCM_MAKE_INT (ref (-> sdl-event wheel) x))
                                      (SCM_MAKE_INT (ref (-> sdl-event wheel) y))
                                      (?: (== (ref (-> sdl-event wheel) direction) SDL_MOUSEWHEEL_NORMAL) 'normal 'flipped))))
     ((SDL_JOYAXISMOTION)
      (call-global-handler 'joystick-axis-motion
                           (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jaxis) which))
                                      (SCM_MAKE_INT (ref (-> sdl-event jaxis) axis))
                                      (SCM_MAKE_INT (ref (-> sdl-event jaxis) value)))))
     ((SDL_JOYBALLMOTION)
      (call-global-handler 'joystick-ball-motion
                           (SCM_LIST4 (SCM_MAKE_INT (ref (-> sdl-event jball) which))
                                      (SCM_MAKE_INT (ref (-> sdl-event jball) ball))
                                      (SCM_MAKE_INT (ref (-> sdl-event jball) xrel))
                                      (SCM_MAKE_INT (ref (-> sdl-event jball) yrel)))))
     ((SDL_JOYHATMOTION)
      (call-global-handler 'joystick-hat-motion
                           (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jhat) which))
                                      (SCM_MAKE_INT (ref (-> sdl-event jhat) hat))
                                      (hat-position->symbol (ref (-> sdl-event jhat) value)))))
     ((SDL_JOYBUTTONDOWN SDL_JOYBUTTONUP)
      (call-global-handler (?: (== (ref (-> sdl-event jbutton) type) SDL_JOYBUTTONDOWN)
                               'joystick-button-down
                               'joystick-button-up)
                           (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jbutton) which))
                                      (SCM_MAKE_INT (ref (-> sdl-event jbutton) button))
                                      (state->symbol (ref (-> sdl-event jbutton) state)))))
     ((SDL_JOYDEVICEADDED SDL_JOYDEVICEREMOVED)
      (call-global-handler (?: (== (-> sdl-event type) SDL_JOYDEVICEADDED)
                               'joystick-device-added
                               'joystick-device-removed)
                           (SCM_LIST1 (SCM_MAKE_INT (ref (-> sdl-event jdevice) which)))))
     ((SDL_CONTROLLERAXISMOTION)
      (call-global-handler 'controller-axis-motion
                           (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event caxis) which))
                                      (axis->symbol (ref (-> sdl-event caxis) axis))
                                      (SCM_MAKE_INT (ref (-> sdl-event caxis) value)))))
     ((SDL_CONTROLLERBUTTONDOWN SDL_CONTROLLERBUTTONUP)
      (call-global-handler (?: (== (-> sdl-event type) SDL_CONTROLLERBUTTONDOWN)
                               'controller-button-down
                               'controller-button-up)
                           (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event cbutton) which))
                                      (button->symbol (ref (-> sdl-event cbutton) button))
                                      (state->symbol (ref (-> sdl-event cbutton) state)))))
     ((SDL_CONTROLLERDEVICEADDED SDL_CONTROLLERDEVICEREMOVED SDL_CONTROLLERDEVICEREMAPPED)
      (call-global-handler (?: (== (-> sdl-event type) SDL_CONTROLLERDEVICEADDED)
                               'controller-device-added
                               (?: (== (-> sdl-event type) SDL_CONTROLLERDEVICEREMOVED)
                                   'controller-device-removed
                                   'controller-device-remapped))
                           (SCM_LIST1 (SCM_MAKE_INT (ref (-> sdl-event cdevice) which)))))
     ((SDL_AUDIODEVICEADDED SDL_AUDIODEVICEREMOVED)
      (call-global-handler (?: (== (-> sdl-event type) SDL_AUDIODEVICEADDED)
                               'audio-device-added
                               'audio-device-removed)
                           (SCM_LIST2 (SCM_MAKE_INT (ref (-> sdl-event adevice) which))
                                      (SCM_MAKE_BOOL (ref (-> sdl-event adevice) iscapture)))))
     ((SDL_QUIT)
      (call-global-handler 'quit SCM_NIL))
     ((SDL_FINGERMOTION SDL_FINGERDOWN SDL_FINGERUP)
      (call-global-handler (?: (== (-> sdl-event type) SDL_FINGERMOTION)
                               'finger-motion
                               (?: (== (-> sdl-event type) SDL_FINGERDOWN)
                                   'finger-down
                                   'finger-up))
                           (Scm_List (Scm_MakeInteger (ref (-> sdl-event tfinger) touchId))
                                     (Scm_MakeInteger (ref (-> sdl-event tfinger) fingerId))
                                     (Scm_MakeFlonum (ref (-> sdl-event tfinger) x))
                                     (Scm_MakeFlonum (ref (-> sdl-event tfinger) y))
                                     (Scm_MakeFlonum (ref (-> sdl-event tfinger) dx))
                                     (Scm_MakeFlonum (ref (-> sdl-event tfinger) dy))
                                     (Scm_MakeFlonum (ref (-> sdl-event tfinger) pressure))
                                     NULL)))
     ((SDL_MULTIGESTURE)
      (call-global-handler 'multi-gesture
                           (Scm_List (Scm_MakeInteger (ref (-> sdl-event mgesture) touchId))
                                     (Scm_MakeFlonum (ref (-> sdl-event mgesture) dTheta))
                                     (Scm_MakeFlonum (ref (-> sdl-event mgesture) dDist))
                                     (Scm_MakeFlonum (ref (-> sdl-event mgesture) x))
                                     (Scm_MakeFlonum (ref (-> sdl-event mgesture) y))
                                     (SCM_MAKE_INT (ref (-> sdl-event mgesture) numFingers))
                                     NULL)))
     ((SDL_DOLLARGESTURE SDL_DOLLARRECORD)
      (call-global-handler (?: (== (-> sdl-event type) SDL_DOLLARGESTURE)
                               'dollar-gesture
                               'dollar-record)
                           (Scm_List (Scm_MakeInteger (ref (-> sdl-event dgesture) touchId))
                                     (Scm_MakeInteger (ref (-> sdl-event dgesture) gestureId))
                                     (SCM_MAKE_INT (ref (-> sdl-event dgesture) numFingers))
                                     (Scm_MakeFlonum (ref (-> sdl-event dgesture) error))
                                     (Scm_MakeFlonum (ref (-> sdl-event dgesture) x))
                                     (Scm_MakeFlonum (ref (-> sdl-event dgesture) y))
                                     NULL)))
     ((SDL_DROPFILE SDL_DROPTEXT)
      (call-window-handler (ref (-> sdl-event drop) windowID)
                           (?: (== (-> sdl-event type) SDL_DROPFILE) 'drop-file 'drop-text)
                           (SCM_LIST1 (SCM_MAKE_STR_COPYING (ref (-> sdl-event drop) file))))
      (SDL_free (ref (-> sdl-event drop) file)))
     ((SDL_DROPBEGIN SDL_DROPCOMPLETE)
      (call-window-handler (ref (-> sdl-event drop) windowID)
                           (?: (== (-> sdl-event type) SDL_DROPBEGIN) 'drop-begin 'drop-complete)
                           SCM_NIL))
     (else
      (cond
        ((== (-> sdl-event type) graviton-event-type)
         (case (ref (-> sdl-event user) code)
           ((GRAVITON_EXCEPTION_CODE)
            (let* ((exception (ref (-> sdl-event user) data1)))
              (Scm_Raise exception 0)))
           ((GRAVITON_UNCAUGHT_EXCEPTION_CODE)
            (Scm_Write (SCM_OBJ (ref (-> sdl-event user) data1)) (SCM_OBJ SCM_CURERR) SCM_WRITE_DISPLAY)
            (Scm_Printf SCM_CURERR "\n")
            (Scm_Error "async failed, but the exception wasn't caught."))
           ((GRAVITON_MML_FINISH_CODE)
            (Mix_HookMusic NULL NULL)
            (set! music-last-finished-tick (SDL_GetTicks)))
           ((GRAVITON_APPLY_CODE)
            (let* ((procs (ref (-> sdl-event user) data1))
                   (args (ref (-> sdl-event user) data2)))
              (for-each (lambda (proc)
                          (Scm_ApplyRec proc args))
                        procs)))
           ((GRAVITON_UPDATE_WINDOWS)
            (for-each (lambda (win)
                        (%call-window-handler win 'update SCM_NIL))
                      grv-windows)
            (update-window-contents))
           ) ;; end of case (for graviton-event-type)
         ))  ;; end of cond
      ))     ;; end of case
   )         ;; end of define-cfn

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

 (define-cfn update-windows-callback (interval::Uint32 param::void*)
   ::Uint32
   (let* ((event::SDL_Event))
     (set! (ref event type) graviton-event-type
           (ref event user code) GRAVITON_UPDATE_WINDOWS)
     (SDL_PushEvent (& event))
     (return (/ 1000 frame-per-second))))

 (.define MUSIC_FINISHED_GRACE_PERIOD 100)
 )  ;; end of inline-stub

(define-cproc notify-exception (exception)
  ::<void>
  (let* ((event::SDL_Event))
    (set! (ref event type) graviton-event-type
          (ref event user code) GRAVITON_EXCEPTION_CODE
          (ref event user data1) exception)
    (SDL_PushEvent (& event))))

(define-cproc event-loop-running? ()
  ::<boolean>
  (return (event-loop-running?)))

(define-cproc frame-per-second ()
  ::<int>
  (return frame-per-second))

(define-cproc set-frame-per-second! (fps::<int>)
  ::<void>
  (let* ((t::Uint32 (cast Uint32 (floor (/ 1000.0 fps)))))
    (set! frame-per-second fps)))

(define-cproc start-global-event-loop (thunk)
  ::<void>
  (set-event-loop-status true)
  (SDL_StartTextInput)

  (Scm_ApplyRec0 thunk)
  (let* ((callback-id::SDL_TimerID (SDL_AddTimer 0 update-windows-callback NULL)))
    (while (logior (not (SCM_NULLP grv-windows))
                   (is-repl-running?)
                   (or (playing-mml?)
                       (Mix_PlayingMusic)
                       (Mix_Playing -1)
                       (< (SDL_GetTicks) (+ music-last-finished-tick MUSIC_FINISHED_GRACE_PERIOD))))
      (let* ((event::SDL_Event))
        (when (SDL_WaitEvent (& event))
          (process-event (& event)))))
    (SDL_RemoveTimer callback-id))

  (set-event-loop-status false))

(define-cproc global-handler-table ()
  (return global-handler-table))

(define (set-global-handler! event proc)
  (hash-table-set! (global-handler-table) event proc))

(define-macro (define-on-window-event-macros :rest events)
  `(begin
     ,@(map (lambda (event)
              (let1  on-event (string->symbol (format "on-~a" event))
                `(define-syntax ,on-event
                   (syntax-rules ()
                     ((_ window (arg ...) body ...)
                      (set-window-handler! window ',event
                        (lambda (window arg ...)
                          body ...)))))))
            events)))

(define-macro (define-on-global-event-macros :rest events)
  `(begin
     ,@(map (lambda (event)
              (let1  on-event (string->symbol (format "on-~a" event))
                `(define-syntax ,on-event
                   (syntax-rules ()
                     ((_ (arg ...) body ...)
                      (set-global-handler! ',event
                        (lambda (arg ...)
                          body ...)))))))
            events)))

(define-on-window-event-macros
  window-shown
  window-hidden
  window-exposed
  window-moved
  window-resized
  window-size-changed
  window-minimized
  window-maximized
  window-restored
  window-enter
  window-leave
  window-focus-gained
  window-focus-lost
  window-close
  window-take-focus
  window-hit-test
  key-down
  key-up
  text-editing
  text-input
  mouse-motion
  mouse-button-down
  mouse-button-up
  mouse-wheel
  drop-file
  drop-text
  drop-begin
  drop-complete
  update
  ) ;; end of define-on-window-event-macros

(define-on-global-event-macros
  joystick-axis-motion
  joystick-ball-motion
  joystick-hat-motion
  joystick-button-down
  joystick-button-up
  joystick-device-added
  joystick-device-removed
  controller-axis-motion
  controller-button-down
  controller-button-up
  controller-device-added
  controller-device-removed
  controller-device-remapped
  audio-device-added
  audio-device-removed
  quit
  finger-motion
  finger-down
  finger-up
  multi-gesture
  dollar-gesture
  dollar-record
  ) ;; end of define-on-global-event-macros

(define (grv-main thunk :key (repl? (is-interactive?)))
  (cond
    ((event-loop-running?)
     (thunk))
    (else
     (guard (e (else (destroy-all-windows)
                     (kill-repl)
                     (kill-scheduler)
                     (raise e)))
       (start-global-event-loop (lambda ()
                                  (run-scheduler)
                                  (when repl?
                                    (run-grv-repl (current-input-port)
                                                  (current-output-port)
                                                  (current-error-port)))
                                  (thunk)))
       (shutdown-scheduler)))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_)
     (grv-main (lambda () #f)))
    ((_ expr ...)
     (grv-main (lambda () expr ...)))))

(define (grv-exit)
  (destroy-all-windows)
  (exit-repl))


;;;
;;; Image & Window coordinate utilities
;;;

(define-cproc pixel-size (gimage::<graviton-image>)
  ::(<double> <double>)
  (return (image-pixel-width gimage)
          (image-pixel-height gimage)))

(define (pixel-width image)
  (values-ref (pixel-size image) 0))

(define (pixel-height image)
  (values-ref (pixel-size image) 1))

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

(define-cproc center-point (window-or-image)
  ::<list>
  (cond
    ((GRV_IMAGE_P window-or-image)
     (let* ((param::TransformParam* (& (-> (GRV_IMAGE_PTR window-or-image) param))))
       (return (SCM_LIST2 (Scm_MakeFlonum (/ (+ (-> param left) (-> param right)) 2.0))
                          (Scm_MakeFlonum (/ (+ (-> param top) (-> param bottom)) 2.0))))))
    ((GRV_WINDOW_P window-or-image)
     (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR window-or-image)))
       (return (SCM_LIST2 (Scm_MakeFlonum (/ (-> gwin logical-width) 2.0))
                          (Scm_MakeFlonum (/ (-> gwin logical-height) 2.0))))))
    (else
     (Scm_Error "<graviton-window> or <graviton-image> required, but got %S" window-or-image))))

(define (center-x window-or-image)
  (list-ref (center-point window-or-image) 0))

(define (center-y window-or-image)
  (list-ref (center-point window-or-image) 1))

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


;;;
;;; Draw vector graphics
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
    (image-coordinate gimage x0 y0 (& ix0) (& iy0))
    (image-coordinate gimage x1 y1 (& ix1) (& iy1))
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

(define (draw-point image x y color :key (thickness 0))
  (cond
    ((= thickness 0)
     (draw-rect image x y (pixel-width image) (pixel-height image) color))
    (else
     (draw-circle image x y (/. thickness 2) color :fill? #t))))

(define (draw-rect image x y w h color :key (fill? #f) (thickness 0))
  (cond
    ((= thickness 0)
     (%draw-rect image x y w h color fill?))
    (else
     (let ((x0 x)
           (y0 y)
           (x1 (+ x w))
           (y1 (+ x h)))
       (draw-polygon image
                     (list (make-point x0 y0)
                           (make-point x0 y1)
                           (make-point x1 y1)
                           (make-point x1 y0))
                     color
                     :fill? fill?
                     :thinkness thickness)))))

(define (draw-line image points color :key (thickness 0))
(cond
  ((= thickness 0)
   (%draw-line image points color))
  ((null? points)
   #f)
  (else
   (draw-point image (point-x (car points)) (point-y (car points)) color :thickness thickness)
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
          (draw-point image x1 y1 color :thickness thickness)
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
          (draw-point image x1 y1 color :thickness thickness)
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
          (draw-point image x1 y1 color :thickness thickness)
          (loop (car points) (cdr points)))))))))

(define (draw-polygon image points color :key (fill? #f) (thickness 0))
  (cond
    ((= thickness 0)
     (%draw-polygon image points color fill?))
    ((null? points)
     #f)
    (else
     (draw-line image (cons (last points) points) color :thickness thickness)
     (when fill?
       (%draw-polygon image points color #t)))))

(define (draw-circle image
                     center-x
                     center-y
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
           (loop (+ t dt) (cons (rotate-point x y) points))))))))


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


;;;
;;; MML
;;;

(inline-stub
 (define-cfn playing-mml? ()
   ::bool :static
   (return (!= (Mix_GetMusicHookData) NULL)))

 (define-cfn enqueue-mml-music-context! (gcontext::GrvMMLMusicContext*)
   ::void :static
   (lock-global-var)
   (set! (aref (ref mml-music-context-queue buf) (ref mml-music-context-queue end)) gcontext
         (ref mml-music-context-queue end) (% (+ (ref mml-music-context-queue end) 1)
                                              (ref mml-music-context-queue length)))
   (when (== (ref mml-music-context-queue start) (ref mml-music-context-queue end))
     (let* ((newlen::int (* (ref mml-music-context-queue length) 2))
            (newbuf::GrvMMLMusicContext** (SCM_NEW_ARRAY (.type GrvMMLMusicContext*) newlen)))
       (dotimes (i (ref mml-music-context-queue length))
         (set! (aref newbuf i) (aref (ref mml-music-context-queue buf) (% (+ i (ref mml-music-context-queue start))
                                                                          (ref mml-music-context-queue length)))))
       (set! (ref mml-music-context-queue start) 0
             (ref mml-music-context-queue end) (ref mml-music-context-queue length)
             (ref mml-music-context-queue buf) newbuf
             (ref mml-music-context-queue length) newlen)))
   (unlock-global-var))

 (define-cfn dequeue-mml-music-context! ()
   ::GrvMMLMusicContext* :static
   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (lock-global-var)
     (unless (== (ref mml-music-context-queue start) (ref mml-music-context-queue end))
       (set! gcontext (aref (ref mml-music-context-queue buf) (ref mml-music-context-queue start))
             (ref mml-music-context-queue start) (% (+ (ref mml-music-context-queue start) 1)
                                                    (ref mml-music-context-queue length))))
     (unlock-global-var)
     (return gcontext)))

 (define-cfn retain-soundlet! (gcontext::GrvMMLMusicContext* gsoundlet::GrvSoundlet* pos::int)
   ::void
   (unless gsoundlet
     (return))
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (when (== (aref (-> gcontext soundlet-contexts) i) NULL)
       (let* ((sctx:: GrvSoundletContext* (SCM_NEW (.type GrvSoundletContext))))
         (set! (-> sctx start-position) pos
               (-> sctx soundlet) gsoundlet
               (aref (-> gcontext soundlet-contexts) i) sctx))
       (when (== (-> gsoundlet type) SOUNDLET_COMPOSITE)
         (let* ((gcomposite::GrvCompositeSoundlet* (ref (-> gsoundlet data) composite)))
           (dotimes (j (-> gcomposite num-children))
             (retain-soundlet! gcontext (aref (-> gcomposite children) j) pos))))
       (return)))

   (let* ((new-size::int (* (-> gcontext num-soundlet-contexts) 2))
          (new-contexts::GrvSoundletContext** (SCM_NEW_ARRAY (.type GrvSoundletContext*) new-size)))
     (dotimes (i new-size)
       (cond
         ((< i (-> gcontext num-soundlet-contexts))
          (set! (aref new-contexts i) (aref (-> gcontext soundlet-contexts) i)))
         (else
          (set! (aref new-contexts i) NULL))))
     (set! (-> gcontext soundlet-contexts) new-contexts
           (-> gcontext num-soundlet-contexts) new-size)
     (retain-soundlet! gcontext gsoundlet pos)))

 (define-cfn release-soundlet! (gcontext::GrvMMLMusicContext* gsoundlet::GrvSoundlet*)
   ::void
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (let* ((sctx::GrvSoundletContext* (aref (-> gcontext soundlet-contexts) i)))
       (when (and sctx (== (-> sctx soundlet) gsoundlet))
         (set! (aref (-> gcontext soundlet-contexts) i) NULL)
         (return)))))

 (define-cfn inc-buffer! (buf::int16_t* index::int dv::double)
   ::void
   (let* ((v::int (+ (aref buf index) (cast int (floor (* dv INT16_MAX))))))
     (cond
       ((< INT16_MAX v)
        (set! (aref buf index) INT16_MAX))
       ((< v INT16_MIN)
        (set! (aref buf index) INT16_MIN))
       (else
        (set! (aref buf index) v)))))

 (define-cfn compute-tone-silent (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (return 0))

 (define-cfn compute-tone-sine (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (set! v (+ v (* (aref amps i) (sin (* 2 M_PI (aref freqs i) (/ rel-pos 44100.0)))))))
     (return v)))

 (define-cfn compute-tone-square50 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-square12 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len12::int (/ len 8)))
         (cond
           ((< (% rel-pos len) len12)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-square25 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len25::int (/ len 4)))
         (cond
           ((< (% rel-pos len) len25)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-triangle (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (half::int (/ len 2)))
         (cond
           ((< (% rel-pos len) half)
            (set! v (+ v (- (/ (* 2.0 amp (% rel-pos len)) half) amp))))
           (else
            (set! v (+ v (+ (/ (* -2.0 amp (% rel-pos len)) half) (* 3.0 amp))))))))
     (return v)))

 (define-cfn compute-tone-sawtooth (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq)))))
         (set! v (+ v (- (/ (* 2.0 amp (% rel-pos len)) len) amp)))))
     (return v)))

 (define-cfn compute-tone-long-noise (freqs::double* amps::double* num-freqs::int rel-pos::int dummy::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (when (< len50 1)
           (set! len50 1))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) NOISE_TABLE_SIZE))))))
           (else
            (set! v (- v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) NOISE_TABLE_SIZE)))))))))
     (return v)))

 (.define SHORT_NOISE_SIZE 100)
 (define-cfn compute-tone-short-noise (freqs::double* amps::double* num-freqs::int rel-pos::int dummy::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (when (< len50 1)
           (set! len50 1))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) SHORT_NOISE_SIZE))))))
           (else
            (set! v (- v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) SHORT_NOISE_SIZE)))))))))
     (return v)))

 (define-cfn render-tone (buf::int16_t* index::int gcontext::GrvMMLMusicContext* gsoundlet-context::GrvSoundletContext*)
   ::void
   (let* ((gsoundlet::GrvSoundlet* (-> gsoundlet-context soundlet))
          (gtone::GrvToneSoundlet* (ref (-> gsoundlet data) tone))
          (start-pos::int (-> gsoundlet-context start-position))
          (cur-pos::int (-> gcontext position))
          (rel-pos::int (- cur-pos start-pos))
          (length::int (-> gsoundlet length))
          (attack-time::int (-> gtone attack-time))
          (decay-time::int (-> gtone decay-time))
          (sustain-level::double (-> gtone sustain-level))
          (release-time::int (-> gtone release-time)))
     (when (< rel-pos 0)
       (return))
     (when (== rel-pos (- length 1))
       (retain-soundlet! gcontext (-> gsoundlet next) (+ cur-pos 1)))
     (when (<= (+ length release-time) rel-pos)
       (release-soundlet! gcontext (-> gsoundlet-context soundlet))
       (return))

     (let* ((v::double 0))
       (case (-> gtone type)
         ((TONE_SILENT)
          (set! v (compute-tone-silent (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SINE)
          (set! v (compute-tone-sine (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE50)
          (set! v (compute-tone-square50 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE12)
          (set! v (compute-tone-square12 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE25)
          (set! v (compute-tone-square25 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_TRIANGLE)
          (set! v (compute-tone-triangle (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SAWTOOTH)
          (set! v (compute-tone-sawtooth (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_LONG_NOISE)
          (set! v (compute-tone-long-noise (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos start-pos)))
         ((TONE_SHORT_NOISE)
          (set! v (compute-tone-short-noise (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos start-pos))))
       (cond
         ((< rel-pos attack-time)
          (set! v (* (/ 1.0 attack-time) rel-pos v)))
         ((and (<= attack-time rel-pos) (< rel-pos (+ attack-time decay-time)))
          (set! v (* (+ (* (/ (- sustain-level 1.0) decay-time) (- rel-pos attack-time)) 1.0) v)))
         ((and (<= (+ attack-time decay-time) rel-pos) (< rel-pos length))
          (set! v (* sustain-level v)))
         (else
          (set! v (* (+ (* (/ (- sustain-level) release-time) (- rel-pos length)) sustain-level) v))))
       (inc-buffer! buf index (* (-> gtone left-volume) v))
       (inc-buffer! buf (+ index 1) (* (-> gtone right-volume) v)))))

 (define-cfn render-composite (buf::int16_t* index::int gcontext::GrvMMLMusicContext* gsoundlet-context::GrvSoundletContext*)
   ::void
   (let* ((gsoundlet::GrvSoundlet* (-> gsoundlet-context soundlet))
          (gcomposite::GrvCompositeSoundlet* (ref (-> gsoundlet data) composite))
          (start-pos::int (-> gsoundlet-context start-position))
          (cur-pos::int (-> gcontext position))
          (rel-pos::int (- cur-pos start-pos))
          (length::int (-> gsoundlet length)))
     (when (== rel-pos (- length 1))
       (retain-soundlet! gcontext (-> gsoundlet next) (+ cur-pos 1)))
     (when (<= length rel-pos)
       (release-soundlet! gcontext (-> gsoundlet-context soundlet)))))

 (define-cfn render-context (buf::int16_t* index::int gcontext::GrvMMLMusicContext*)
   ::void :static
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (let* ((sctx::GrvSoundletContext* (aref (-> gcontext soundlet-contexts) i)))
       (unless (== sctx NULL)
         (cond
           ((== (-> sctx soundlet type) SOUNDLET_TONE)
            (render-tone buf index gcontext sctx))
           ((== (-> sctx soundlet type) SOUNDLET_COMPOSITE)
            (render-composite buf index gcontext sctx)))))))

 (define-cfn fill-audio-stream (udata::void* stream::Uint8* len::int)
   ::void :static
   (memset stream 0 len)

   (when mml-paused?
     (return))

   (let* ((buf::int16_t* (cast int16_t* stream))
          (buf-length::int (/ len 2))
          (index::int 0)
          (gcontext::GrvMMLMusicContext* (cast GrvMMLMusicContext* udata))
          (pos::int (-> gcontext position))
          (pos-end::int (+ pos (/ buf-length 2)))
          (i::int 0))
     (while (< i buf-length)
       (render-context buf i gcontext)
       (inc! (-> gcontext position))
       (set! i (+ i 2)))

     (let* ((num::int 0))
       (dotimes (i (-> gcontext num-soundlet-contexts))
         (unless (== (aref (-> gcontext soundlet-contexts) i) NULL)
           (inc! num)))
       (when (== num 0)
         (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
           (set-future-result! gfuture (SCM_LIST1 'finished) false))

         (let* ((next-gcontext::GrvMMLMusicContext* (dequeue-mml-music-context!)))
           (cond
             (next-gcontext
              (set! (-> gcontext position) (-> next-gcontext position)
                    (-> gcontext soundlet-contexts) (-> next-gcontext soundlet-contexts)
                    (-> gcontext num-soundlet-contexts) (-> next-gcontext num-soundlet-contexts)
                    (-> gcontext future) (-> next-gcontext future)))
             (else
              (let* ((event::SDL_Event))
                (set! (ref event type) graviton-event-type
                      (ref event user code) GRAVITON_MML_FINISH_CODE)
                (SDL_PushEvent (& event))))))))))

 (define-cfn stop-mml ()
   ::void
   (let* ((gcontext::GrvMMLMusicContext* (Mix_GetMusicHookData)))
     (when gcontext
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (set-future-result! gfuture (SCM_LIST1 'stopped) false))))
   (Mix_HookMusic NULL NULL)

   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (while (= gcontext (dequeue-mml-music-context!))
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (set-future-result! gfuture (SCM_LIST1 'cancelled) false)))))

 (define-cfn pause-mml ()
   ::void
   (lock-global-var)
   (set! mml-paused? true)
   (unlock-global-var))

 (define-cfn resume-mml ()
   ::void
   (lock-global-var)
   (set! mml-paused? false)
   (unlock-global-var))

 (define-cfn paused-mml? ()
   ::bool
   (lock-global-var)
   (let* ((paused?::bool mml-paused?))
     (unlock-global-var)
     (return paused?)))

 (define-cfn compute-total-length (gsoundlet::GrvSoundlet*)
   ::int
   (let* ((cursor::GrvSoundlet* gsoundlet)
          (len::int 0))
     (while cursor
       (set! len (+ len (-> gsoundlet length))
             cursor (-> cursor next)))
     (return len)))
 ) ;; end of inline-stub

(define-cproc make-soundlet (type
                             freqs::<f64vector>
                             amps::<f64vector>
                             left-volume::<double>
                             right-volume::<double>
                             sec::<double>
                             attack-time::<double>
                             decay-time::<double>
                             sustain-level::<double>
                             release-time::<double>)
  ::<graviton-soundlet>
  (unless (== (SCM_F64VECTOR_SIZE freqs) (SCM_F64VECTOR_SIZE amps))
    (Scm_Error "freqs and amps must be the same size"))
  (let* ((length::int (cast int (round (* 44100 sec))))
         (gtone::GrvToneSoundlet* (SCM_NEW GrvToneSoundlet))
         (gsoundlet::GrvSoundlet* (SCM_NEW GrvSoundlet))
         (tone-type::ToneType))
    (cond
      ((SCM_EQ type 'silent)
       (set! tone-type TONE_SILENT))
      ((or (SCM_EQ type 'sine)
           (SCM_EQ type 'sin))
       (set! tone-type TONE_SINE))
      ((or (SCM_EQ type 'square)
           (SCM_EQ type 'square50))
       (set! tone-type TONE_SQUARE50))
      ((or (SCM_EQ type 'square12)
           (SCM_EQ type 'square125))
       (set! tone-type TONE_SQUARE12))
      ((SCM_EQ type 'square25)
       (set! tone-type TONE_SQUARE25))
      ((SCM_EQ type 'triangle)
       (set! tone-type TONE_TRIANGLE))
      ((SCM_EQ type 'sawtooth)
       (set! tone-type TONE_SAWTOOTH))
      ((or (SCM_EQ type 'noise)
           (SCM_EQ type 'long-noise))
       (set! tone-type TONE_LONG_NOISE))
      ((SCM_EQ type 'short-noise)
       (set! tone-type TONE_SHORT_NOISE))
      (else
       (Scm_Error "Invalid tone type: %S" type)))

    (set! (-> gtone type) tone-type
          (-> gtone freqs) (SCM_NEW_ATOMIC_ARRAY (.type double) (SCM_F64VECTOR_SIZE freqs))
          (-> gtone amps) (SCM_NEW_ATOMIC_ARRAY (.type double) (SCM_F64VECTOR_SIZE amps))
          (-> gtone num-freqs) (SCM_F64VECTOR_SIZE freqs)
          (-> gtone left-volume) left-volume
          (-> gtone right-volume) right-volume
          (-> gtone attack-time) (cast int (round (* 44100 attack-time)))
          (-> gtone decay-time) (cast int (round (* 44100 decay-time)))
          (-> gtone sustain-level) sustain-level
          (-> gtone release-time) (cast int (round (* 44100 release-time))))
    (memcpy (-> gtone freqs) (SCM_F64VECTOR_ELEMENTS freqs) (* (sizeof (.type double)) (SCM_F64VECTOR_SIZE freqs)))
    (memcpy (-> gtone amps) (SCM_F64VECTOR_ELEMENTS amps) (* (sizeof (.type double)) (SCM_F64VECTOR_SIZE amps)))
    (set! (-> gsoundlet next) NULL
          (-> gsoundlet type) SOUNDLET_TONE
          (-> gsoundlet length) length
          (ref (-> gsoundlet data) tone) gtone)
    (return gsoundlet)))

(define-cproc compose-soundlets (soundlets::<list>)
  ::<graviton-soundlet>
  (let* ((gcomposite::GrvCompositeSoundlet* (SCM_NEW GrvCompositeSoundlet))
         (gsoundlet::GrvSoundlet* (SCM_NEW GrvSoundlet)))
    (set! (-> gcomposite num-children) (Scm_Length soundlets)
          (-> gcomposite children) (SCM_NEW_ARRAY (.type GrvSoundlet*) (-> gcomposite num-children)))
    (let* ((i::int 0)
           (max-len::int 0))
      (for-each (lambda (child)
                  (unless (GRV_SOUNDLET_P child)
                    (Scm_Error "<graviton-soundlet> required, but got %S" child))
                  (let* ((child-gsoundlet::GrvSoundlet* (GRV_SOUNDLET_PTR child))
                         (len::int 0))
                    (set! (aref (-> gcomposite children) i) child-gsoundlet
                          len (compute-total-length child-gsoundlet))
                    (when (< max-len len)
                      (set! max-len len)))
                  (pre++ i))
                soundlets)
      (set! (-> gsoundlet next) NULL
            (-> gsoundlet length) max-len
            (-> gsoundlet type) SOUNDLET_COMPOSITE
            (ref (-> gsoundlet data) composite) gcomposite))
    (return gsoundlet)))

(define-cproc append-soundlet! (gsoundlet1::<graviton-soundlet> gsoundlet2::<graviton-soundlet>)
  ::<void>
  (set! (-> gsoundlet1 next) gsoundlet2))

(define-cproc play-soundlet (gsoundlet::<graviton-soundlet>)
  (Mix_HaltMusic)

  (let* ((gcontext::GrvMMLMusicContext* (SCM_NEW (.type GrvMMLMusicContext)))
         (future (make-future)))
    (set! (-> gcontext position) 0
          (-> gcontext num-soundlet-contexts) 16
          (-> gcontext soundlet-contexts) (SCM_NEW_ARRAY (.type GrvSoundletContext*) (-> gcontext num-soundlet-contexts))
          (-> gcontext future) future)
    (dotimes (i (-> gcontext num-soundlet-contexts))
      (set! (aref (-> gcontext soundlet-contexts) i) NULL))
    (retain-soundlet! gcontext gsoundlet 0)
    (cond
      ((playing-mml?)
       (enqueue-mml-music-context! gcontext))
      (else
       (Mix_HookMusic fill-audio-stream gcontext)))
    (return future)))

(define (pitch n)
  (* 440 (expt 2 (/. (- n 69) 12))))

(define-record-type (<envelope> (pseudo-rtd <list>))
  make-envelope envelope?
  (attack-time envelope-attack-time)
  (decay-time envelope-decay-time)
  (sustain-level envelope-sustain-level)
  (release-time envelope-release-time))

(define (generate-make-simple-tone type)
  (lambda (freq velocity vols sec envelope)
    (make-soundlet type
                   (make-f64vector 1 freq)
                   (make-f64vector 1 velocity)
                   (list-ref vols 0)
                   (list-ref vols 1)
                   sec
                   (envelope-attack-time envelope)
                   (envelope-decay-time envelope)
                   (envelope-sustain-level envelope)
                   (envelope-release-time envelope))))

(define make-default-tone (generate-make-simple-tone 'sine))
(define (make-silent sec)
  (make-soundlet 'silent #f64() #f64() 0 0 sec 0 0 0 0))
(define default-envelope (make-envelope 0 0 1.0 0))

(define (merge-soundlet seq)
  (fold (lambda (former latter)
          (when latter
            (append-soundlet! former latter))
          former)
        #f
        seq))

(define (compile-mml context seq mml cont)
  (match mml
    (()
     (cont context seq))
    ((('wave type freq vel sec) rest ...)
     (let ((make-tone (generate-make-simple-tone type))
           (vols (assoc-ref context 'stereo-balance '(1.0 1.0)))
           (envelope (assoc-ref context 'envelope default-envelope)))
       (compile-mml
         context
         (cons (make-tone freq vel vols sec envelope) seq)
         rest
         cont)))
    ((('note n v sec) rest ...)
     (let ((make-tone (assoc-ref context 'make-tone make-default-tone))
           (vols (assoc-ref context 'stereo-balance '(1.0 1.0)))
           (envelope (assoc-ref context 'envelope default-envelope)))
       (compile-mml
         context
         (cons (make-tone (pitch n) v vols sec envelope) seq)
         rest
         cont)))
    ((('rest sec) rest ...)
     (compile-mml
       context
       (cons (make-silent sec) seq)
       rest
       cont))
    ((('compose mmls ...) rest ...)
     (compile-mml
       context
       (cons (compose-soundlets (filter values (map (lambda (mml)
                                                      (compile-mml context '() mml (lambda (_ seq)
                                                                                     (merge-soundlet seq))))
                                                    mmls)))
             seq)
       rest
       cont))
    ((('begin mml ...) rest ...)
     (let1 current-context (map (lambda (pair)
                                  (list-copy pair))
                                context)
       (compile-mml context seq mml (lambda (context seq)
                                      (compile-mml current-context seq rest cont)))))
    ((('stereo-balance left right) rest ...)
     (compile-mml
       (assoc-set! context 'stereo-balance (list left right))
       seq
       rest
       cont))
    ((('tone (? symbol? type)) rest ...)
     (compile-mml
       (assoc-set! context 'make-tone (generate-make-simple-tone type))
       seq
       rest
       cont))
    ((('tone (freq-coff amp) ...) rest ...)
     (compile-mml
       (assoc-set! context 'make-tone (lambda (freq velocity vols sec envelope)
                                        (make-soundlet type
                                                       (list->f64vector (map (cut (* freq <>)) freq-coff))
                                                       (list->f64vector amp)
                                                       (list-ref vols 0)
                                                       (list-ref vols 1)
                                                       sec
                                                       (envelope-attack-time envelope)
                                                       (envelope-decay-time envelope)
                                                       (envelope-sustain-level envelope)
                                                       (envelope-release-time envelope))))
       seq
       rest
       cont))
    ((('envelope attack-time decay-time sustain-level release-time) rest ...)
     (compile-mml
       (assoc-set! context 'envelope (make-envelope attack-time decay-time sustain-level release-time))
       seq
       rest
       cont))
    ((('length n) rest ...)
     (compile-mml
       (assoc-set! context 'length (/ 1 n))
       seq
       rest
       cont))
    ((('velocity v) rest ...)
     (compile-mml
       (assoc-set! context 'velocity v)
       seq
       rest
       cont))
    ((('tempo n) rest ...)
     (compile-mml
       (assoc-set! context 'tempo-factor (* (/ 60 n) 4))
       seq
       rest
       cont))
    ((('tempo n m) rest ...)
     (compile-mml
       (assoc-set! context 'tempo-factor (* (/ 60 n) m))
       seq
       rest
       cont))
    ((('octave n) rest ...)
     (compile-mml
       (assoc-set! context 'octave n)
       seq
       rest
       cont))
    (((? symbol? note-spec) rest ...)
     (compile-mml
       context
       (cons (note-spec->soundlet context (symbol->string note-spec)) seq)
       rest
       cont))
    (((? string? note-spec) rest ...)
     (compile-mml
       context
       (cons (note-spec->soundlet context note-spec) seq)
       rest
       cont))
    (else
     (errorf "Invalied mml: ~s" mml))))

(define parse-note
  (let* ((basic-note ($or ($string "c")
                          ($string "d")
                          ($string "e")
                          ($string "f")
                          ($string "g")
                          ($string "a")
                          ($string "b")
                          ($string "r")))
         (note ($do (bn basic-note)
                    (qual ($optional ($or ($string "+") ($string "-"))))
                    ($return (string-append (rope->string bn) (or (rope->string qual) "")))))
         (chord ($many note))
         (len ($do (digits ($many ($one-of #[\d])))
                   (qual ($many ($string "+")))
                   ($return (cond
                              ((null? digits)
                               #f)
                              (else
                               digits (let1 basic-len (/ 1 (string->number (apply string digits)))
                                        (* basic-len (sum-ec (: i (+ (length qual) 1))
                                                             (/ 1 (expt 2 i))))))))))
         (chord+len ($do (ns chord)
                         (l ($optional len))
                         ($return (list ns l)))))
    (lambda (str)
      (call-with-input-string str
        (lambda (in)
          (begin0
            (peg-parse-port chord+len in)
            (unless (eof-object? (read-char in))
              (errorf "Invalid note: ~s" str))))))))

(define note->pitch
  (let1 table '(("c-" . -1)
                ("c" . 0)
                ("c+" . 1)
                ("d-" . 1)
                ("d" . 2)
                ("d+" . 3)
                ("e-" . 3)
                ("e" . 4)
                ("e+" . 5)
                ("f-" . 4)
                ("f" . 5)
                ("f+" . 6)
                ("g-" . 6)
                ("g" . 7)
                ("g+" . 8)
                ("a-" . 8)
                ("a" . 9)
                ("a+" . 10)
                ("b-" . 10)
                ("b" . 11)
                ("b+" . 12))
    (lambda (octave note)
      (if (equal? note "r")
          #f
          (+ (* 12 (+ octave 1)) (assoc-ref table note))))))

(define (note-spec->soundlet context str)
  (match-let1 ((notes ...) len) (parse-note str)
    (let ((sec (* (assoc-ref context 'tempo-factor 4)
                  (or len (assoc-ref context 'length (/ 1 4)))))
          (octave (assoc-ref context 'octave 4))
          (make-tone (assoc-ref context 'make-tone make-default-tone))
          (vols (assoc-ref context 'stereo-balance '(1.0 1.0)))
          (velocity (assoc-ref context 'velocity 1.0))
          (envelope (assoc-ref context 'envelope default-envelope)))
      (receive (pitches _) (fold2 (lambda (note pitches prev-pitch)
                                    (let1 pitch-num (let1 p (note->pitch octave note)
                                                      (cond
                                                        ((not p)
                                                         #f)
                                                        ((<= prev-pitch p)
                                                         p)
                                                        (else
                                                         (+ p 12))))
                                      (values (cons pitch-num pitches)
                                              (or pitch-num prev-pitch))))
                                  '()
                                  (least-fixnum)
                                  notes)
        (compose-soundlets (map (lambda (pitch-num)
                                  (if pitch-num
                                      (make-tone (pitch pitch-num) velocity vols sec envelope)
                                      (make-silent sec)))
                                pitches))))))

(define (play-mml mml)
  (play-soundlet (compile-mml '() '() mml (lambda (context seq)
                                            (merge-soundlet seq)))))

(define (beep :optional (freq 2000) (len 0.1))
  (play-mml `((wave square ,freq 1.0 ,len))))


;;;
;;; Music
;;;

(inline-stub
 (define-cfn finalize-music (z data::void*)
   ::void
   (when (GRV_MUSIC_P z)
     (let* ((gmusic::GrvMusic* (GRV_MUSIC_PTR z)))
       (Mix_FreeMusic (-> gmusic music))
       (set! (-> gmusic music) NULL))))

 (define-cfn set-playing-music-context! (music-context::GrvMusicContext*)
   ::GrvMusicContext*
   (lock-global-var)
   (let* ((prev::GrvMusicContext* playing-music-context))
     (set! playing-music-context music-context)
     (unlock-global-var)
     (return prev)))

 (define-cfn finish-music ()
   ::void :static
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (set-future-result! gfuture (SCM_LIST1 'finished) false)
         (set! music-last-finished-tick (SDL_GetTicks))))))

 (define-cfn stop-music ()
   ::void
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (set-future-result! gfuture (SCM_LIST1 'stopped) false)
         (set! music-last-finished-tick (SDL_GetTicks)))))
   (Mix_HaltMusic))
 ) ;; end of inline-stub

(define-cproc load-music (filename::<const-cstring>)
  (let* ((mmusic::Mix_Music* (Mix_LoadMUS filename)))
    (unless mmusic
      (Scm_Error "Mix_LoadMUS failed: %s" (Mix_GetError)))
    (let* ((gmusic::GrvMusic* (SCM_NEW (.type GrvMusic)))
           (music (MAKE_GRV_MUSIC gmusic)))
      (set! (-> gmusic music) mmusic)
      (Scm_RegisterFinalizer music finalize-music NULL)
      (return music))))

(define-cproc play-music (music)
  (unless (GRV_MUSIC_P music)
    (Scm_Error "<graviton-music> required, but got %S" music))

  (stop-mml)
  (stop-music)

  (let* ((music-context::GrvMusicContext* (SCM_NEW (.type GrvMusicContext))))
    (set! (-> music-context music) music
          (-> music-context future) (make-future))
    (set-playing-music-context! music-context)
    (when (< (Mix_PlayMusic (-> (GRV_MUSIC_PTR music) music) 0) 0)
      (set-playing-music-context! NULL)
      (Scm_Error "Mix_PlayMusic failed: %s" (Mix_GetError)))
    (return (-> music-context future))))

(define-cproc stop-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (stop-mml))
    (else
     (stop-music))))

(define-cproc pause-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (pause-mml))
    (else
     (Mix_PauseMusic))))

(define-cproc resume-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (resume-mml))
    (else
     (Mix_ResumeMusic))))

(define-cproc playing-music? ()
  ::<boolean>
  (cond
    ((playing-mml?)
     (return true))
    (else
     (return (Mix_PlayingMusic)))))

(define-cproc paused-music? ()
  ::<boolean>
  (cond
    ((playing-mml?)
     (return (paused-mml?)))
    (else
     (return (Mix_PausedMusic)))))

(define-cproc set-music-volume! (volume::<int>)
  ::<void>
  (Mix_VolumeMusic volume))

(define-cproc music-volume ()
  ::<int>
  (return (Mix_VolumeMusic -1)))

;;;
;;; Sound
;;;

(inline-stub
 (define-cfn finalize-sound (z data::void*)
   ::void
   (when (GRV_SOUND_P z)
     (Mix_FreeChunk (-> (GRV_SOUND_PTR z) chunk))))

 (define-cfn set-playing-sound-context! (channel::int sound-context::GrvSoundContext*)
   ::GrvSoundContext*
   (lock-global-var)
   (let* ((prev::GrvSoundContext* (aref playing-sound-contexts channel)))
     (set! (aref playing-sound-contexts channel) sound-context)
     (unlock-global-var)
     (return prev)))

 (define-cfn find-available-channel ()
   ::int
   (let* ((i::int))
     (lock-global-var)
     (for ((set! i 0) (< i CHANNEL_SIZE) (inc! i))
       (when (== (aref playing-sound-contexts i) NULL)
         (break)))
     (unlock-global-var)

     (cond
       ((== i CHANNEL_SIZE)
        (return -1))
       (else
        (return i)))))

 (define-cfn stop-sound (channel::int)
   ::void
   (let* ((sound-context::GrvSoundContext* (set-playing-sound-context! channel NULL)))
     (when sound-context
       (set-future-result! (GRV_FUTURE_PTR (-> sound-context future)) 'stopped false))
     (Mix_HaltChannel channel)))

 (define-cfn finish-sound (channel::int)
   ::void :static
   (let* ((sound-context::GrvSoundContext* (set-playing-sound-context! channel NULL)))
     (when sound-context
       (set-future-result! (GRV_FUTURE_PTR (-> sound-context future)) 'finished false))))
 ) ;; end of inline-stub

(define-cproc load-sound (filename::<const-cstring>)
  (let* ((chunk::Mix_Chunk* (Mix_LoadWAV filename)))
    (unless chunk
      (Scm_Error "Mix_LoadWAV failed: %s" (Mix_GetError)))
    (let* ((gsound::GrvSound* (SCM_NEW (.type GrvSound)))
           (sound (MAKE_GRV_SOUND gsound)))
      (set! (-> gsound chunk) chunk)
      (Scm_RegisterFinalizer sound finalize-sound NULL)
      (return sound))))

(define-cproc play-sound (sound :key (channel #f))
  ::(<top> <int>)
  (unless (GRV_SOUND_P sound)
    (Scm_Error "<graviton-sound> required, but got %S" sound))

  (let* ((which::int))
    (cond
      ((SCM_INTP channel)
       (set! which (SCM_INT_VALUE channel))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which)))
      ((SCM_FALSEP channel)
       (set! which (find-available-channel))
       (when (< which 0)
         (Scm_Error "no available channels")))
      (else
       (Scm_Error "<integer> or #f required, but got %S" channel)))

    (stop-sound which)
    (let* ((sound-context::GrvSoundContext* (SCM_NEW (.type GrvSoundContext))))
      (set! (-> sound-context sound) sound
            (-> sound-context future) (make-future))
      (set-playing-sound-context! which sound-context)
      (when (< (Mix_PlayChannel which (-> (GRV_SOUND_PTR sound) chunk) 0) 0)
        (set-playing-sound-context! which NULL)
        (Scm_Error "Mix_PlayChannel failed: %s" (Mix_GetError)))
      (return (-> sound-context future) which))))

(define-cproc stop-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (let* ((i::int))
       (for ((set! i 0) (< i CHANNEL_SIZE) (inc! i))
         (stop-sound i))))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (stop-sound which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc playing-sound? (:optional (channel #f))
  ::<boolean>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Playing -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Playing which))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc pause-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (Mix_Pause -1))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Pause which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc resume-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (Mix_Resume -1))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Resume which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc paused-sound? (:optional (channel #f))
  ::<boolean>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Paused -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Paused which))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc set-sound-volume! (arg0 :optional arg1)
  ::<void>
  (cond
    ((and (SCM_INTP arg0)
          (SCM_UNBOUNDP arg1))
     ;; arg0: volume
     (Mix_Volume -1 (SCM_INT_VALUE arg0)))
    ((and (SCM_FALSEP arg0)
          (SCM_INTP arg1))
     ;; arg0: #f, arg1: volume
     (Mix_Volume -1 (SCM_INT_VALUE arg0)))
    ((and (SCM_INTP arg0)
          (SCM_INTP arg1))
     ;; arg0: channel, arg1: volume
     (let* ((which::int (SCM_INT_VALUE arg0)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Volume which (SCM_INT_VALUE arg1))))
    ((SCM_UNBOUNDP arg1)
     (Scm_Error "invalid argument: %S" arg0))
    (else
     (Scm_Error "invalid arguments: %S %S" arg0 arg1))))

(define-cproc sound-volume (:optional (channel #f))
  ::<int>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Volume -1 -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Volume which -1))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(include "graviton/enum2sym.scm")

(compile-stub :pkg-config '("sdl2" "SDL2_mixer SDL2_image") :cflags "-g")


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

(set! (setter music-volume) set-music-volume!)
(set! (setter sound-volume) set-sound-volume!)