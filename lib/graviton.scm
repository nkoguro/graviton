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
  (use control.thread-pool)
  (use data.queue)
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
          <graviton-future>

          <point>
          make-point
          point?
          point-x
          point-y

          async
          async*
          async-apply
          async*-apply
          main-async
          await
          yield
          await-sleep

          make-window
          destroy-window
          clear-window-sprites!
          send-close-window-event
          window-size
          window-width
          window-height
          set-window-size!
          window-resolution
          window-resolution-width
          window-resolution-height
          set-window-resolution!
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

          next-event
          event-for-each
          frame-per-second
          set-frame-per-second!
          actual-frame-per-second
          load-average
          frame-duration
          start-global-event-loop
          set-global-handler!
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
          image-rgba-pixels
          set-image-rgba-pixels!

          make-sprite
          set-sprite-image!
          sprite-image
          set-sprite-center!
          sprite-center
          set-sprite-z!
          sprite-z
          set-sprite-angle!
          sprite-angle
          set-sprite-zoom!
          sprite-zoom
          set-sprite-visible!
          sprite-visible?

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
                              srcrect::SDL_Rect*
                              center-x::double
                              center-y::double
                              z::double
                              angle::double
                              zoom-x::double
                              zoom-y::double
                              visible::bool)))

   (define-ctype GrvWindow::(.struct
                             (window::SDL_Window*
                              renderer::SDL_Renderer*
                              sprites
                              icon
                              param::TransformParam
                              virtual-width::int
                              virtual-height::int
                              zoom::double
                              handler-table)))

   (define-ctype ScratchArea::(.struct
                               (x::int
                                y::int
                                w::int
                                h::int
                                data::char*)))

   (define-ctype EventLoopStatus::(.struct
                                   (lock::SDL_SpinLock
                                    running?::bool)))

   (define-ctype GrvFuture::(.struct
                             (lock::SDL_mutex*
                              cond::SDL_cond*
                              result
                              exception
                              message::char*
                              continuation
                              consumed?::bool)))
   ) ;; end of declcode

  (define-cvar main-thread-id::SDL_threadID :static)
  (define-cvar grv-windows :static SCM_NIL)
  (define-cvar event-loop-status::EventLoopStatus)
  (define-cvar frame-per-second::int :static 30)
  (define-cvar event-loop-ticks::Uint32 :static 33)
  (define-cvar actual-frame-per-second::double :static)
  (define-cvar frame-duration::double :static)
  (define-cvar load-average::double :static)
  (define-cvar global-handler-table :static)
  (define-cvar repl-thread :static)
  (define-cvar repl-channel :static)
  (define-cvar graviton-event-type::Uint32 :static)

  (define-cptr <graviton-window> :private
    "GrvWindow*" "GravitonWindowClass" "GRV_WINDOW_P" "MAKE_GRV_WINDOW" "GRV_WINDOW_PTR")

  (define-cptr <graviton-image> :private
    "GrvImage*" "GravitonImageClass" "GRV_IMAGE_P" "MAKE_GRV_IMAGE" "GRV_IMAGE_PTR")

  (define-cptr <graviton-texture> :private
    "GrvTexture*" "GravitonTextureClass" "GRV_TEXTURE_P" "MAKE_GRV_TEXTURE" "GRV_TEXTURE_PTR")

  (define-cptr <graviton-sprite> :private
    "GrvSprite*" "GravitonSpriteClass" "GRV_SPRITE_P" "MAKE_GRV_SPRITE" "GRV_SPRITE_PTR")

  (define-cptr <graviton-future> :private
    "GrvFuture*" "GravitonFutureClass" "GRV_FUTURE_P" "MAKE_GRV_FUTURE" "GRV_FUTURE_PTR")

  (.define GRAVITON_EXCEPTION_CODE 1)
  (.define GRAVITON_UNCAUGHT_EXCEPTION_CODE 2)
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

    (set! graviton-event-type (SDL_RegisterEvents 1))
    (when (== graviton-event-type #xffffffff)
      (Scm_Error "SDL_RegisterEvents failed: %s" (SDL_GetError)))

    (set! main-thread-id (SDL_ThreadID)
          global-handler-table (Scm_MakeHashTableSimple SCM_HASH_EQ 16)
          repl-thread SCM_FALSE
          repl-channel SCM_FALSE)
    (set! (ref event-loop-status lock) 0
          (ref event-loop-status running?) false)
    ;; (SDL_LogSetPriority SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_DEBUG)
    )

  (initcode
   (initialize-libs))
  ) ;; end of inline-stub

(define-cproc current-ticks ()
  ::<int>
  (return (SDL_GetTicks)))


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

  (.define MAX_MESSAGE_LENGTH 1024)
  ) ;; end of inline-stub

(define-cproc make-future ()
  (let* ((gfuture::GrvFuture* (SCM_NEW GrvFuture))
         (obj (MAKE_GRV_FUTURE gfuture)))
    (set! (-> gfuture lock) (SDL_CreateMutex)
          (-> gfuture cond) (SDL_CreateCond)
          (-> gfuture result) SCM_FALSE
          (-> gfuture exception) SCM_FALSE
          (-> gfuture message) NULL
          (-> gfuture continuation) SCM_FALSE
          (-> gfuture consumed?) false)
    (Scm_RegisterFinalizer obj finalize-future NULL)
    (return obj)))

(define-cproc set-future-result! (gfuture::<graviton-future> result)
  ::<void>
  (let* ((cont SCM_FALSE)
         (err?::bool false))
    (SDL_LockMutex (-> gfuture lock))
    (cond
      ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
       (set! (-> gfuture result) result
             cont (-> gfuture continuation))
       (SDL_CondSignal (-> gfuture cond)))
      (else
       (set! err? true)))
    (SDL_UnlockMutex (-> gfuture lock))
    (when err?
      (Scm_Error "result has been already set in <graviton-future>"))
    (when (SCM_PROCEDUREP cont)
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (Scm_ApplyRec cont (SCM_LIST2 result SCM_FALSE)))))

(define-cproc set-future-exception! (gfuture::<graviton-future> exception error-message::<const-cstring>)
  ::<void>
  (let* ((cont SCM_FALSE)
         (err?::bool false))
    (SDL_LockMutex (-> gfuture lock))
    (cond
      ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
       (let* ((msg::char* (malloc MAX_MESSAGE_LENGTH)))
         (bzero msg MAX_MESSAGE_LENGTH)
         (strncpy msg error-message (- MAX_MESSAGE_LENGTH 1))
         (set! (-> gfuture exception) exception
               (-> gfuture message) msg
               cont (-> gfuture continuation))
         (SDL_CondSignal (-> gfuture cond))))
      (else
       (set! err? true)))
    (SDL_UnlockMutex (-> gfuture lock))
    (when err?
      (Scm_Error "result has been already set in <graviton-future>"))
    (when (SCM_PROCEDUREP cont)
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (Scm_ApplyRec cont (SCM_LIST2 SCM_FALSE exception)))))

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

(define-cproc set-future-continuation! (gfuture::<graviton-future> cont)
  ::<void>
  (let* ((result SCM_FALSE)
         (exception SCM_FALSE))
    (SDL_LockMutex (-> gfuture lock))
    (set! (-> gfuture continuation) cont
          result (-> gfuture result)
          exception (-> gfuture exception))
    (SDL_UnlockMutex (-> gfuture lock))
    (unless (and (SCM_FALSEP result) (SCM_FALSEP exception))
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (Scm_ApplyRec cont (SCM_LIST2 result exception)))))

(define-cproc on-main-thread? ()
  ::<boolean>
  (return (== main-thread-id (SDL_ThreadID))))

(define graviton-thread-pool (make-parameter (make-thread-pool 1)))

(define main-thread-thunk-queue (make-mtqueue))

(define (submit-main-thread thunk)
  (enqueue! main-thread-thunk-queue thunk))

(define (submit-thread-pool thunk)
  (add-job! (graviton-thread-pool) thunk))

(define (submit thunk :optional (type (if (on-main-thread?) 'main 'pool)))
  (let1 %submit (case type
                  ((main) submit-main-thread)
                  ((pool) submit-thread-pool)
                  (else
                   (errorf "type must be 'main or 'pool, but got ~s" thread)))
    (%submit thunk)))

(define (process-thunk-on-main-thread available-ticks)
  (let1 start-ticks (current-ticks)
    (let loop ()
      (cond
        ((and (< (- (current-ticks) start-ticks) available-ticks)
              (dequeue/wait! main-thread-thunk-queue (max (/. (- available-ticks (- (current-ticks) start-ticks)) 1000)
                                                          0)))
         => (lambda (thunk)
              (thunk)
              (loop)))
        (else
         (queue-length main-thread-thunk-queue))))))

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
  (%async-apply 'pool proc args))

(define (async*-apply proc :rest args)
  (%async-apply 'main proc args))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...)))))

(define-syntax async*
  (syntax-rules ()
    ((_ expr ...)
     (async*-apply (lambda () expr ...)))))

(define (await future)
  (cond
    ((is-a? future <graviton-future>)
     (let1 %submit (if (on-main-thread?)
                       submit-main-thread
                       submit-thread-pool)
       (shift cont
         (set-future-continuation! future
                                   (lambda (result exception)
                                     (let1 thunk (cond
                                                   (result
                                                    (apply cont result))
                                                   (exception
                                                    (raise exception))
                                                   (else
                                                    (errorf "[BUG] result and exception aren't specified.")))
                                       (%submit thunk)))))))
    (else
     future)))

(define (await-sleep sec)
  (let1 %submit (if (on-main-thread?)
                    submit-main-thread
                    submit-thread-pool)
    (shift cont
      (add-timer! sec (lambda ()
                        (%submit cont))))))

(define (report-uncaught-future-exceptions messages)
  (for-each (lambda (msg)
              (display msg (current-error-port))
              (newline (current-error-port)))
            messages)
  (error "async failed, but the exception wasn't caught."))

(define (yield)
  (shift cont (submit cont)))

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
            (-> image texture_alist) SCM_NIL
            (ref (-> image update_rect) x) 0
            (ref (-> image update_rect) y) 0
            (ref (-> image update_rect) w) 0
            (ref (-> image update_rect) h) 0)
      (return image))))

(define-cproc set-image-border! (gimage::<graviton-image> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (compute-transform-param (& (-> gimage param)) (-> gimage surface w) (-> gimage surface h) left top right bottom false))

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
               (spr-center-x::int)
               (spr-center-y::int)
               (spr-w::double (* (-> gsprite srcrect w) zoom-x (-> gwin zoom)))
               (spr-h::double (* (-> gsprite srcrect h) zoom-y (-> gwin zoom)))
               (dstrect::SDL_Rect))
          (window-coordinate gwin (-> gsprite center_x) (-> gsprite center_y) (& spr-center-x) (& spr-center-y))
          (set! (ref dstrect x) (- spr-center-x (cast int (round (/ spr-w 2.0))))
                (ref dstrect y) (- spr-center-y (cast int (round (/ spr-h 2.0))))
                (ref dstrect w) (cast int (round spr-w))
                (ref dstrect h) (cast int (round spr-h)))
          (SDL_RenderCopyEx (-> gwin renderer)
                            texture
                            (-> gsprite srcrect)
                            (& dstrect)
                            angle-deg
                            NULL
                            flip)))))

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
                           :key
                           (image #f)
                           center
                           (z::<double> 0.0)
                           (angle::<double> 0.0)
                           zoom
                           (visible::<boolean> #t))
  (unless (GRV_WINDOW_P window)
    (Scm_Error "window must be <graviton-window>, but got %S" window))
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image))
    (Scm_Error "image must be <graviton-image> or #f, but got %S" image))
  (unless (or (SCM_UNBOUNDP center)
              (and (SCM_LISTP center)
                   (SCM_REALP (Scm_ListRef center 0 SCM_UNBOUND))
                   (SCM_REALP (Scm_ListRef center 1 SCM_UNBOUND))))
    (Scm_Error "center must be (<real> <real>), but got %S" center))
  (unless (or (SCM_UNBOUNDP zoom)
              (SCM_REALP zoom)
              (and (SCM_LISTP zoom)
                   (SCM_REALP (Scm_ListRef zoom 0 SCM_UNBOUND))
                   (SCM_REALP (Scm_ListRef zoom 1 SCM_UNBOUND))))
    (Scm_Error "zoom must be <real> or (<real> <real>), but got %S" zoom))

  (let* ((gsprite::GrvSprite* (SCM_NEW (.type GrvSprite)))
         (center-x::double 0.0)
         (center-y::double 0.0)
         (zoom-x::double 1.0)
         (zoom-y::double 1.0)
         (srcrect::SDL_Rect* NULL))
    (when (SCM_LISTP center)
      (set! center-x (Scm_GetDouble (Scm_ListRef center 0 SCM_UNBOUND))
            center-y (Scm_GetDouble (Scm_ListRef center 1 SCM_UNBOUND))))
    (cond
      ((SCM_REALP zoom)
       (set! zoom-x (Scm_GetDouble zoom)
             zoom-y (Scm_GetDouble zoom)))
      ((SCM_LISTP zoom)
       (set! zoom-x (Scm_GetDouble (Scm_ListRef zoom 0 SCM_UNBOUND))
             zoom-y (Scm_GetDouble (Scm_ListRef zoom 1 SCM_UNBOUND)))))

    (when (GRV_IMAGE_P image)
      (set! srcrect (SCM_NEW SDL_Rect)
            (-> srcrect x) 0
            (-> srcrect y) 0
            (-> srcrect w) (-> (GRV_IMAGE_PTR image) surface w)
            (-> srcrect h) (-> (GRV_IMAGE_PTR image) surface h)))
    (set! (-> gsprite window) window
          (-> gsprite image) image
          (-> gsprite center-x) center-x
          (-> gsprite center-y) center-y
          (-> gsprite z) z
          (-> gsprite srcrect) srcrect
          (-> gsprite angle) angle
          (-> gsprite zoom-x) zoom-x
          (-> gsprite zoom-y) zoom-y
          (-> gsprite visible) visible)
    (unless (SCM_FALSEP image)
      (retain-texture window (GRV_IMAGE_PTR image)))
    (let* ((sprite (MAKE_GRV_SPRITE gsprite)))
      (Scm_RegisterFinalizer sprite finalize-sprite NULL)
      (insert-window-sprite sprite)
      (return sprite))))

(define-cproc set-sprite-image! (gsprite::<graviton-sprite> image)
  ::<void>
  (unless (or (SCM_FALSEP image)
              (GRV_IMAGE_P image))
    (Scm_Error "image must be <graviton-image> or #f, but got %S" image))

  (let* ((srcrect::SDL_Rect* NULL))
    (cond
      ((GRV_IMAGE_P image)
       (let* ((gimage::GrvImage* (GRV_IMAGE_PTR image)))
         (retain-texture (-> gsprite window) gimage)
         (set! srcrect (SCM_NEW SDL_Rect)
               (-> srcrect x) 0
               (-> srcrect y) 0
               (-> srcrect w) (-> gimage surface w)
               (-> srcrect h) (-> gimage surface h))))
      (else
       (when (GRV_IMAGE_P (-> gsprite image))
         (release-texture (-> gsprite window) (GRV_IMAGE_PTR (-> gsprite image))))))

    (set! (-> gsprite image) image
          (-> gsprite srcrect) srcrect)))

(define-cproc sprite-image (gsprite::<graviton-sprite>)
  ::<top>
  (return (-> gsprite image)))

(define-cproc set-sprite-center! (gsprite::<graviton-sprite> center)
  ::<void>
  (unless (and (SCM_LISTP center)
               (SCM_REALP (Scm_ListRef center 0 SCM_UNBOUND))
               (SCM_REALP (Scm_ListRef center 1 SCM_UNBOUND)))
    (Scm_Error "center must be (<real> <real>), but got %S" center))
  (set! (-> gsprite center-x) (Scm_GetDouble (Scm_ListRef center 0 SCM_UNBOUND))
        (-> gsprite center-y) (Scm_GetDouble (Scm_ListRef center 1 SCM_UNBOUND))))

(define-cproc sprite-center (gsprite::<graviton-sprite>)
  ::<list>
  (return (SCM_LIST2 (Scm_MakeFlonum (-> gsprite center-x)) (Scm_MakeFlonum (-> gsprite center-y)))))

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

  (define-cfn set-window-resolution! (gwin::GrvWindow* actual-width::int actual-height::int virtual-width::int virtual-height::int)
    ::void
    (let* ((zoom-w::double)
           (zoom-h::double))
      (set! zoom-w (/ (cast double actual-width) (cast double virtual-width))
            zoom-h (/ (cast double actual-height) (cast double virtual-height))
            (-> gwin virtual-width) virtual-width
            (-> gwin virtual-height) virtual-height
            (-> gwin zoom) (?: (< zoom-w zoom-h) zoom-w zoom-h))
      (let* ((rect::SDL_Rect))
        (set! (ref rect w) (cast int (round (* virtual-width (-> gwin zoom))))
              (ref rect h) (cast int (round (* virtual-height (-> gwin zoom))))
              (ref rect x) (/ (- actual-width (ref rect w)) 2)
              (ref rect y) (/ (- actual-height (ref rect h)) 2))
        (when (< (SDL_RenderSetClipRect (-> gwin renderer) (& rect)) 0)
          (Scm_Error "SDL_RenderSetClipRect failed: %s" (SDL_GetError))))))
  ) ;; end of inline-stub

(define-cproc make-window (title::<const-cstring> width::<int> height::<int> :key (resizable?::<boolean> #f) (icon #f) (shown?::<boolean> #t) (maximized?::<boolean> #f) (minimized?::<boolean> #f) (fullscreen?::<boolean> #f))
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
                                               width
                                               height
                                               flags))
      (unless (-> gwin window)
        (Scm_Error "SDL_CreateWindow failed: %s" (SDL_GetError)))

      (when (GRV_IMAGE_P icon)
        (SDL_SetWindowIcon (-> gwin window) (-> (GRV_IMAGE_PTR icon) surface)))

      (set! (-> gwin renderer) (SDL_CreateRenderer (-> gwin window) -1 0))
      (unless (-> gwin renderer)
        (Scm_Error "SDL_CreateRenderer failed: %s" (SDL_GetError)))

      (let* ((actual-width::int)
             (actual-height::int))
        (SDL_GetWindowSize (-> gwin window) (& actual-width) (& actual-height))
        (compute-transform-param (& (-> gwin param)) actual-width actual-height 0 0 (- width 1) (- height 1) true)
        (set-window-resolution! gwin actual-width actual-height width height))

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

(define-cproc set-window-border! (gwin::<graviton-window> top::<double> right::<double> bottom::<double> left::<double>)
  ::<void>
  (let* ((width::int)
         (height::int))
    (SDL_GetWindowSize (-> gwin window) (& width) (& height))
    (compute-transform-param (& (-> gwin param)) width height left top right bottom true)))

(define-cproc clear-window-sprites! (gwin::<graviton-window>)
  ::<void>
  (for-each (lambda (sprite)
              (let* ((gsprite::GrvSprite* (GRV_SPRITE_PTR gsprite)))
                (invalidate-sprite gsprite)))
            (-> gwin sprites))
  (set! (-> gwin sprites) SCM_NIL))

(define-cproc destroy-window (gwin::<graviton-window>)
  ::<void>
  (destroy-window gwin))

(define-cproc destroy-all-windows ()
  ::<void>
  (for-each (lambda (obj)
              (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR obj)))
                (destroy-window gwin)))
            grv-windows))

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
  ::(<int> <int>)
  (let* ((w::int)
         (h::int))
    (SDL_GetWindowSize (-> gwin window) (& w) (& h))
    (return w h)))

(define (window-width window)
  (values-ref (window-size window) 0))

(define (window-height window)
  (values-ref (window-size window) 1))

(define-cproc set-window-size! (gwin::<graviton-window> width::<int> height::<int>)
  ::<void>
  (SDL_SetWindowSize (-> gwin window) width height)
  (compute-transform-param (& (-> gwin param))
                           width
                           height
                           (ref (-> gwin param) left)
                           (ref (-> gwin param) top)
                           (ref (-> gwin param) right)
                           (ref (-> gwin param) bottom)
                           true)
  (set-window-resolution! gwin width height (-> gwin virtual-width) (-> gwin virtual-height)))

(define-cproc window-resolution (gwin::<graviton-window>)
  ::(<int> <int>)
  (return (-> gwin virtual-width) (-> gwin virtual-height)))

(define (window-resolution-width window)
  (values-ref (window-resolution window) 0))

(define (window-resolution-height window)
  (values-ref (window-resolution window) 1))

(define-cproc set-window-resolution! (gwin::<graviton-window> virtual-width::<int> virtual-height::<int>)
  ::<void>
  (let* ((actual-width::int)
         (actual-height::int))
    (SDL_GetWindowSize (-> gwin window) (& actual-width) (& actual-height))
    (set-window-resolution! gwin actual-width actual-height virtual-width virtual-height)))

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

(define-cproc reflect-resized-window-parameter (gwin::<graviton-window> actual-width::<int> actual-height::<int>)
  ::<void>
  (printf "actual-width: %d, actual-height: %d\n" actual-width actual-height)
  (printf "virtual-width: %d, virtual-height: %d\n" (-> gwin virtual-width) (-> gwin virtual-height))
  (printf "left: %f, top: %f, right: %f, bottom: %f\n"
          (ref (-> gwin param) left)
          (ref (-> gwin param) top)
          (ref (-> gwin param) right)
          (ref (-> gwin param) bottom))
  (compute-transform-param (& (-> gwin param))
                           actual-width
                           actual-height
                           (ref (-> gwin param) left)
                           (ref (-> gwin param) top)
                           (ref (-> gwin param) right)
                           (ref (-> gwin param) bottom)
                           true)
  (set-window-resolution! gwin actual-width actual-height (-> gwin virtual-width) (-> gwin virtual-height)))

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
      (receive r (force-future (async*
                                 (eval sexpr ((with-module gauche.internal vm-current-module)))))
        (%set-history-expr! r)
        (apply values r)))))

(define %prompter
  (let1 user-module (find-module 'user)
    (lambda ()
      (let1 m (force-future (async*
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
    (set! frame-per-second fps
          event-loop-ticks t)))

(define-cproc actual-frame-per-second ()
  ::<double>
  (return actual-frame-per-second))

(define-cproc load-average ()
  ::<double>
  (return load-average))

(define-cproc frame-duration (:optional (limit #f))
  ::<double>
  (let* ((max-limit::double))
    (cond
      ((SCM_FALSEP limit)
       (set! max-limit DBL_MAX))
      ((SCM_REALP limit)
       (set! max-limit (Scm_GetDouble limit)))
      (else
       (Scm_Error "<real> required, but got %S" limit)))
    (return (?: (< frame-duration max-limit) frame-duration max-limit))))

(define-cproc %start-global-event-loop (thunk)
  ::<void>
  (set-event-loop-status true)
  (SDL_StartTextInput)

  (Scm_ApplyRec0 thunk)
  (set! actual-frame-per-second 0.0
        load-average 0.0)
  (let* ((frame::int 0)
         (backlog::int 0)
         (backlog-acc::int 0)
         (start-ticks::int (SDL_GetTicks))
         (last-frame-ticks::int))
    (while (logior (not (SCM_NULLP grv-windows)) (is-repl-running?) (< 0 backlog))
      (let* ((frame-ticks::int (+ start-ticks (* (+ frame 1) event-loop-ticks))))
        (set! last-frame-ticks (SDL_GetTicks))

        (remove-destroyed-windows)

        (for-each (lambda (win)
                    (%call-window-handler win 'update SCM_NIL))
                  grv-windows)
        (update-window-contents)

        (let* ((event::SDL_Event))
          (while (SDL_PollEvent (& event))
            (process-event (& event))))

        (when (< (SDL_GetTicks) frame-ticks)
          (set! backlog (SCM_INT_VALUE (Scm_EvalRec (SCM_LIST2 'process-thunk-on-main-thread
                                                               (SCM_MAKE_INT (- frame-ticks (SDL_GetTicks))))
                                                    (SCM_OBJ (Scm_FindModule (SCM_SYMBOL 'graviton) 0))))
                backlog-acc (+ backlog-acc backlog)))

        ;; (when (< (SDL_GetTicks) frame-ticks)
        ;;   (SDL_Delay (- frame-ticks (SDL_GetTicks))))

        (pre++ frame)
        (when (== frame frame-per-second)
          (let* ((elapse-in-sec::double (/ (- (SDL_GetTicks) start-ticks) 1000.0)))
            (set! actual-frame-per-second (/ frame-per-second elapse-in-sec)
                  load-average (/ backlog-acc elapse-in-sec)))
          (SDL_LogDebug SDL_LOG_CATEGORY_APPLICATION "fps: %f, load-average: %f" actual-frame-per-second load-average)
          (set! frame 0
                backlog-acc 0
                start-ticks (SDL_GetTicks)))
        (set! frame-duration (/ (- (SDL_GetTicks) last-frame-ticks) 1000.0)))))

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

(define (start-global-event-loop thunk)
  (cond
    ((event-loop-running?)
     (thunk))
    (else
     (guard (e (else (destroy-all-windows)
                     (kill-repl)
                     (kill-scheduler)
                     (raise e)))
       (%start-global-event-loop (lambda ()
                                   (run-scheduler)
                                   (when (is-interactive?)
                                     (run-grv-repl (current-input-port)
                                                   (current-output-port)
                                                   (current-error-port)))
                                   (thunk)))
       (shutdown-scheduler)))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_)
     (start-global-event-loop (lambda () #f)))
    ((_ expr ...)
     (start-global-event-loop (lambda () expr ...)))))

(define (grv-exit)
  (destroy-all-windows)
  (exit-repl))


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
  (cond
    ((= thickness 0)
     (draw-rect image point point color))
    (else
     (draw-circle image point (/. thickness 2) color :fill? #t))))

(define (draw-rect image point0 point1 color :key (fill? #f) (thickness 0))
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

(define (draw-line image points color :key (thickness 0))
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


(define (display-image image :key (fullscreen? #f) (resizable? #f))
  (grv-begin
    (let1 win (make-window (match (command-line)
                             ((program-name args ...)
                              (values-ref (decompose-path program-name) 1))
                             (_
                              "Untitled"))
                           (image-width image)
                           (image-height image)
                           :resizable? resizable?
                           :fullscreen? fullscreen?)
      (let1 sprite (make-sprite win :image image :center (center-point win))
        (on-key-down win (scancode sym mod repeat?)
          (when (eq? scancode 'escape)
            (destroy-window win)))))))


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
(set! (setter sprite-center) set-sprite-center!)
(set! (setter sprite-z) set-sprite-z!)
(set! (setter sprite-angle) set-sprite-angle!)
(set! (setter sprite-zoom) set-sprite-zoom!)
(set! (setter sprite-visible?) set-sprite-visible!)

