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
  (use gauche.hook)
  (use gauche.interactive)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.record)
  (use gauche.selector)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.async)
  (use graviton.color)
  (use graviton.video)
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

(inline-stub
 (declcode
  (.include "SDL.h"
            "SDL_image.h"
            "SDL_mixer.h"
            "float.h"
            "gauche.h"
            "gauche/number.h"
            "gauche/vector.h"
            "graviton.h"
            "stdbool.h"
            "stdio.h"
            "string.h")

  (define-ctype EventLoopStatus::(.struct
                                  (lock::SDL_SpinLock
                                   running?::bool)))
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

 (define-cvar event-loop-status::EventLoopStatus)
 (define-cvar mml-music-context-queue::GrvMMLMusicContextQueue :static)
 (define-cvar mml-paused?::bool :static)
 (define-cvar playing-music-context::GrvMusicContext* :static)
 (define-cvar noise-table::double* :static)
 (define-cvar music-last-finished-tick::Uint32 :static)
 (define-cvar playing-sound-contexts::GrvSoundContext** :static)
 (define-cvar main-thunk-finished?::bool :static)
 (define-cvar Grv_GlobalHandlerTable)

 (define-cptr <graviton-soundlet> :private
   "GrvSoundlet*" "GravitonSoundletClass" "GRV_SOUNDLET_P" "MAKE_GRV_SOUNDLET" "GRV_SOUNDLET_PTR")

 (define-cptr <graviton-music> :private
   "GrvMusic*" "GravitonMusicClass" "GRV_MUSIC_P" "MAKE_GRV_MUSIC" "GRV_MUSIC_PTR")

 (define-cptr <graviton-sound> :private
   "GrvSound*" "GravitonSoundClass" "GRV_SOUND_P" "MAKE_GRV_SOUND" "GRV_SOUND_PTR")

 (.define MML_MUSIC_CONTEXT_INITIAL_LENGTH 16)
 (.define NOISE_TABLE_SIZE 32768)
 (.define CHANNEL_SIZE 16)

 )  ;; end of inline-stub

(include "types.scm")

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

   (set! Grv_GlobalHandlerTable (Scm_MakeHashTableSimple SCM_HASH_EQ 16))
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

   ) ;; end of initialize-libs

 (initcode
  (initialize-libs))
 ) ;; end of inline-stub

;;;
;;;
;;;

(include "enum2sym.scm")
(include "event.scm")
(include "messagebox.scm")
(include "music.scm")
(include "sound.scm")
(include "repl.scm")

;;;
;;; Misc
;;;

(define (grv-main thunk)
  (cond
    ((event-loop-running?)
     (thunk))
    (else
     (guard (e (else (destroy-all-windows)
                     (raise e)))
       (set-main-thunk-finished? #f)
       (start-global-event-loop (lambda ()
                                  (unwind-protect
                                      (thunk)
                                    (set-main-thunk-finished? #t))))))))

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
