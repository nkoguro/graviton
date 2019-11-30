;;;
;;; event.scm - Event handler
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

(select-module graviton.event)

(inline-stub
 (declcode
  (.include "SDL.h"
            "SDL_mixer.h"
            "gauche.h"
            "graviton.h"
            "stdbool.h"))

 (define-cvar Grv_GlobalHandlerTable)
 (define-cvar main-thunk-finished?::bool :static)
 (define-cvar update-all-windows :static SCM_UNDEFINED)
 (define-cvar make-event-hook :static SCM_UNDEFINED)

 (initcode
  (set! Grv_GlobalHandlerTable (Scm_MakeHashTableSimple SCM_HASH_EQ 16)
        Grv_MusicLastFinishedTick 0))

 (define-cfn id->window (window-id::Uint32)
   ::ScmObj
   (for-each (lambda (win)
               (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
                 (when (== (SDL_GetWindowID (-> gwin window)) window-id)
                   (return win))))
             Grv_Windows)
   (return SCM_FALSE))

 (define-cfn window-hook (win event)
   ::ScmObj
   (when (SCM_FALSEP win)
     (return SCM_FALSE))

   (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
     (return (Scm_HashTableRef (SCM_HASH_TABLE (-> gwin hook-table)) event SCM_FALSE))))

 (define-cfn set-window-hook! (win event hook)
   ::void
   (when (SCM_FALSEP win)
     (return))

   (let* ((gwin::GrvWindow* (GRV_WINDOW_PTR win)))
     (Scm_HashTableSet (SCM_HASH_TABLE (-> gwin hook-table)) event hook 0)))

 (define-cfn global-hook (event)
   ::ScmObj
   (return (Scm_HashTableRef (SCM_HASH_TABLE Grv_GlobalHandlerTable) event SCM_FALSE)))

 (define-cfn set-global-hook! (event hook)
   ::void
   (Scm_HashTableSet (SCM_HASH_TABLE Grv_GlobalHandlerTable) event hook 0))

 (define-cfn process-event (sdl-event::SDL_Event*)
   ::void
   (let* ((proc SCM_FALSE)
          (args SCM_NIL))
     (case (-> sdl-event type)
       ((SDL_WINDOWEVENT)
        (let* ((win (id->window (ref (-> sdl-event window) windowID))))
          (when (GRV_WINDOWP win)
            (case (ref (-> sdl-event window) event)
              ((SDL_WINDOWEVENT_MOVED SDL_WINDOWEVENT_RESIZED)
               (set! proc (window-hook win (window-event->symbol (ref (-> sdl-event window) event)))
                     args (SCM_LIST3 win
                                     (SCM_MAKE_INT (ref (-> sdl-event window) data1))
                                     (SCM_MAKE_INT (ref (-> sdl-event window) data2)))))
              ((SDL_WINDOWEVENT_SIZE_CHANGED)
               (Grv_UpdateWindowSize (GRV_WINDOW_PTR win))
               (set! proc (window-hook win (window-event->symbol (ref (-> sdl-event window) event)))
                     args (SCM_LIST3 win
                                     (SCM_MAKE_INT (ref (-> sdl-event window) data1))
                                     (SCM_MAKE_INT (ref (-> sdl-event window) data2)))))
              (else
               (set! proc (window-hook win (window-event->symbol (ref (-> sdl-event window) event)))
                     args (SCM_LIST1 win)))))))
       ((SDL_KEYDOWN SDL_KEYUP)
        (let* ((win (id->window (ref (-> sdl-event key) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win (?: (== (-> sdl-event type) SDL_KEYDOWN) 'key-down 'key-up))
                  args (SCM_LIST5 win
                                  (scancode->symbol (ref (-> sdl-event key) keysym scancode))
                                  (keycode->symbol (ref (-> sdl-event key) keysym sym))
                                  (kmod->symbols (ref (-> sdl-event key) keysym mod))
                                  (SCM_MAKE_BOOL (ref (-> sdl-event key) repeat)))))))
       ((SDL_TEXTEDITING)
        (let* ((win (id->window (ref (-> sdl-event edit) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win 'text-editing)
                  args (SCM_LIST4 win
                                  (SCM_MAKE_STR_COPYING (ref (-> sdl-event edit) text))
                                  (SCM_MAKE_INT (ref (-> sdl-event edit) start))
                                  (SCM_MAKE_INT (ref (-> sdl-event edit) length)))))))
       ((SDL_TEXTINPUT)
        (let* ((win (id->window (ref (-> sdl-event text) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win 'text-input)
                  args (SCM_LIST2 win
                                  (SCM_MAKE_STR_COPYING (ref (-> sdl-event text) text)))))))
       ((SDL_MOUSEMOTION)
        (let* ((win (id->window (ref (-> sdl-event motion) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win 'mouse-motion)
                  args (Scm_List win
                                 (SCM_MAKE_INT (ref (-> sdl-event motion) which))
                                 (mouse-button-state->symbols (ref (-> sdl-event motion) state))
                                 (SCM_MAKE_INT (ref (-> sdl-event motion) x))
                                 (SCM_MAKE_INT (ref (-> sdl-event motion) y))
                                 (SCM_MAKE_INT (ref (-> sdl-event motion) xrel))
                                 (SCM_MAKE_INT (ref (-> sdl-event motion) yrel))
                                 NULL)))))
       ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
        (let* ((win (id->window (ref (-> sdl-event button) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win
                                    (?: (== (-> sdl-event type) SDL_MOUSEBUTTONDOWN) 'mouse-button-down 'mouse-button-up))
                  args (Scm_List win
                                 (SCM_MAKE_INT (ref (-> sdl-event button) which))
                                 (mouse-button->symbol (ref (-> sdl-event button) button))
                                 (SCM_MAKE_INT (ref (-> sdl-event button) clicks))
                                 (SCM_MAKE_INT (ref (-> sdl-event button) x))
                                 (SCM_MAKE_INT (ref (-> sdl-event button) y))
                                 NULL)))))
       ((SDL_MOUSEWHEEL)
        (let* ((win (id->window (ref (-> sdl-event wheel) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win 'mouse-wheel)
                  args (SCM_LIST5 win
                                  (SCM_MAKE_INT (ref (-> sdl-event wheel) which))
                                  (SCM_MAKE_INT (ref (-> sdl-event wheel) x))
                                  (SCM_MAKE_INT (ref (-> sdl-event wheel) y))
                                  (?: (== (ref (-> sdl-event wheel) direction) SDL_MOUSEWHEEL_NORMAL) 'normal 'flipped))))))
       ((SDL_JOYAXISMOTION)
        (set! proc (global-hook 'joystick-axis-motion)
              args (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jaxis) which))
                              (SCM_MAKE_INT (ref (-> sdl-event jaxis) axis))
                              (SCM_MAKE_INT (ref (-> sdl-event jaxis) value)))))
       ((SDL_JOYBALLMOTION)
        (set! proc (global-hook 'joystick-ball-motion)
              args (SCM_LIST4 (SCM_MAKE_INT (ref (-> sdl-event jball) which))
                              (SCM_MAKE_INT (ref (-> sdl-event jball) ball))
                              (SCM_MAKE_INT (ref (-> sdl-event jball) xrel))
                              (SCM_MAKE_INT (ref (-> sdl-event jball) yrel)))))
       ((SDL_JOYHATMOTION)
        (set! proc (global-hook 'joystick-hat-motion)
              args (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jhat) which))
                              (SCM_MAKE_INT (ref (-> sdl-event jhat) hat))
                              (hat-position->symbol (ref (-> sdl-event jhat) value)))))
       ((SDL_JOYBUTTONDOWN SDL_JOYBUTTONUP)
        (set! proc (global-hook (?: (== (ref (-> sdl-event jbutton) type) SDL_JOYBUTTONDOWN)
                                    'joystick-button-down
                                    'joystick-button-up))
              args (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event jbutton) which))
                              (SCM_MAKE_INT (ref (-> sdl-event jbutton) button))
                              (state->symbol (ref (-> sdl-event jbutton) state)))))
       ((SDL_JOYDEVICEADDED SDL_JOYDEVICEREMOVED)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_JOYDEVICEADDED)
                                    'joystick-device-added
                                    'joystick-device-removed))
              args (SCM_LIST1 (SCM_MAKE_INT (ref (-> sdl-event jdevice) which)))))
       ((SDL_CONTROLLERAXISMOTION)
        (set! proc (global-hook 'controller-axis-motion)
              args (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event caxis) which))
                              (axis->symbol (ref (-> sdl-event caxis) axis))
                              (SCM_MAKE_INT (ref (-> sdl-event caxis) value)))))
       ((SDL_CONTROLLERBUTTONDOWN SDL_CONTROLLERBUTTONUP)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_CONTROLLERBUTTONDOWN)
                                    'controller-button-down
                                    'controller-button-up))
              args (SCM_LIST3 (SCM_MAKE_INT (ref (-> sdl-event cbutton) which))
                              (button->symbol (ref (-> sdl-event cbutton) button))
                              (state->symbol (ref (-> sdl-event cbutton) state)))))
       ((SDL_CONTROLLERDEVICEADDED SDL_CONTROLLERDEVICEREMOVED SDL_CONTROLLERDEVICEREMAPPED)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_CONTROLLERDEVICEADDED)
                                    'controller-device-added
                                    (?: (== (-> sdl-event type) SDL_CONTROLLERDEVICEREMOVED)
                                        'controller-device-removed
                                        'controller-device-remapped)))
              args (SCM_LIST1 (SCM_MAKE_INT (ref (-> sdl-event cdevice) which)))))
       ((SDL_AUDIODEVICEADDED SDL_AUDIODEVICEREMOVED)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_AUDIODEVICEADDED)
                                    'audio-device-added
                                    'audio-device-removed))
              args (SCM_LIST2 (SCM_MAKE_INT (ref (-> sdl-event adevice) which))
                              (SCM_MAKE_BOOL (ref (-> sdl-event adevice) iscapture)))))
       ((SDL_QUIT)
        (set! proc (global-hook 'quit)
              args SCM_NIL))
       ((SDL_FINGERMOTION SDL_FINGERDOWN SDL_FINGERUP)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_FINGERMOTION)
                                    'finger-motion
                                    (?: (== (-> sdl-event type) SDL_FINGERDOWN)
                                        'finger-down
                                        'finger-up)))
              args (Scm_List (Scm_MakeInteger (ref (-> sdl-event tfinger) touchId))
                             (Scm_MakeInteger (ref (-> sdl-event tfinger) fingerId))
                             (Scm_MakeFlonum (ref (-> sdl-event tfinger) x))
                             (Scm_MakeFlonum (ref (-> sdl-event tfinger) y))
                             (Scm_MakeFlonum (ref (-> sdl-event tfinger) dx))
                             (Scm_MakeFlonum (ref (-> sdl-event tfinger) dy))
                             (Scm_MakeFlonum (ref (-> sdl-event tfinger) pressure))
                             NULL)))
       ((SDL_MULTIGESTURE)
        (set! proc (global-hook 'multi-gesture)
              args (Scm_List (Scm_MakeInteger (ref (-> sdl-event mgesture) touchId))
                             (Scm_MakeFlonum (ref (-> sdl-event mgesture) dTheta))
                             (Scm_MakeFlonum (ref (-> sdl-event mgesture) dDist))
                             (Scm_MakeFlonum (ref (-> sdl-event mgesture) x))
                             (Scm_MakeFlonum (ref (-> sdl-event mgesture) y))
                             (SCM_MAKE_INT (ref (-> sdl-event mgesture) numFingers))
                             NULL)))
       ((SDL_DOLLARGESTURE SDL_DOLLARRECORD)
        (set! proc (global-hook (?: (== (-> sdl-event type) SDL_DOLLARGESTURE)
                                    'dollar-gesture
                                    'dollar-record))
              args (Scm_List (Scm_MakeInteger (ref (-> sdl-event dgesture) touchId))
                             (Scm_MakeInteger (ref (-> sdl-event dgesture) gestureId))
                             (SCM_MAKE_INT (ref (-> sdl-event dgesture) numFingers))
                             (Scm_MakeFlonum (ref (-> sdl-event dgesture) error))
                             (Scm_MakeFlonum (ref (-> sdl-event dgesture) x))
                             (Scm_MakeFlonum (ref (-> sdl-event dgesture) y))
                             NULL)))
       ((SDL_DROPFILE SDL_DROPTEXT)
        (let* ((win (id->window (ref (-> sdl-event drop) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win (?: (== (-> sdl-event type) SDL_DROPFILE) 'drop-file 'drop-text))
                  args (SCM_LIST2 win (SCM_MAKE_STR_COPYING (ref (-> sdl-event drop) file))))))
        (SDL_free (ref (-> sdl-event drop) file)))
       ((SDL_DROPBEGIN SDL_DROPCOMPLETE)
        (let* ((win (id->window (ref (-> sdl-event drop) windowID))))
          (when (GRV_WINDOWP win)
            (set! proc (window-hook win (?: (== (-> sdl-event type) SDL_DROPBEGIN) 'drop-begin 'drop-complete))
                  args (SCM_LIST1 win)))))
       (else
        (cond
          ((== (-> sdl-event type) Grv_CustomEventType)
           (case (ref (-> sdl-event user) code)
             ((GRV_EVENT_EXCEPTION)
              (let* ((exception (ref (-> sdl-event user) data1)))
                (cond
                  ((SCM_CONDITIONP exception)
                   (Scm_Raise exception 0))
                  ((SCM_STRINGP exception)
                   (Scm_Write exception (SCM_OBJ SCM_CURERR) SCM_WRITE_DISPLAY)
                   (Scm_Printf SCM_CURERR "\n")
                   (Scm_Error "uncaught exception"))
                  (else
                   (Scm_Error "uncaught exception: %S" exception)))))
             ((GRV_EVENT_MML_FINISH)
              (Mix_HookMusic NULL NULL)
              (set! Grv_MusicLastFinishedTick (SDL_GetTicks)))
             ((GRV_EVENT_APPLY)
              (set! proc (ref (-> sdl-event user) data1)
                    args (ref (-> sdl-event user) data2)))
             ((GRV_EVENT_WINDOW_UPDATE)
              (SCM_BIND_PROC update-all-windows "update-all-windows" (SCM_MODULE Grv_GravitonEventModule))
              (set! proc update-all-windows
                    args SCM_NIL))
             ) ;; end of case (for Grv_CustomEventType)
           (Grv_ReleaseObject (ref (-> sdl-event user) data1))
           (Grv_ReleaseObject (ref (-> sdl-event user) data2))
           )) ;; end of cond
        ))    ;; end of case
     (unless (SCM_FALSEP proc)
       (Scm_ApplyRec proc args))
     ) ;; end of let*
   )   ;; end of define-cfn

 (define-cfn update-windows-callback (interval::Uint32 param::void*)
   ::Uint32
   (GRV_SEND_EVENT GRV_EVENT_WINDOW_UPDATE NULL NULL)
   (return (/ 1000 Grv_FramePerSecond)))

 (.define MUSIC_FINISHED_GRACE_PERIOD 200)
 )  ;; end of inline-stub

(include "enum2sym.scm")

(define-cproc notify-exception (exception)
  ::<void>
  (GRV_SEND_EVENT GRV_EVENT_EXCEPTION exception NULL))

(define-cproc set-main-thunk-finished? (flag::<boolean>)
  ::<void>
  (set! main-thunk-finished? flag))

(define-cproc start-global-event-loop (thunk)
  ::<void>
  (Grv_SetEventLoopStatus true)
  (SDL_StartTextInput)

  (GRV_APPLY thunk SCM_NIL)

  (let* ((callback-id::SDL_TimerID (SDL_AddTimer 0 update-windows-callback NULL)))
    (while (logior (SDL_PollEvent NULL)
                   (not main-thunk-finished?)
                   (not (SCM_NULLP Grv_Windows))
                   (or (Grv_IsPlayingMML)
                       (Mix_PlayingMusic)
                       (Mix_Playing -1)
                       (< (SDL_GetTicks) (+ Grv_MusicLastFinishedTick MUSIC_FINISHED_GRACE_PERIOD))))
      (let* ((event::SDL_Event))
        (when (SDL_WaitEvent (& event))
          (process-event (& event)))))
    (SDL_RemoveTimer callback-id))

  (Grv_SetEventLoopStatus false))

(define-cproc window-hook (win event)
  (let* ((hook (window-hook win event)))
    (when (SCM_FALSEP hook)
      (SCM_BIND_PROC make-event-hook "make-event-hook" (SCM_MODULE Grv_GravitonEventModule))
      (set! hook (Scm_ApplyRec1 make-event-hook event))
      (set-window-hook! win event hook))
    (return hook)))

(define-cproc global-hook (event)
  (let* ((hook (global-hook event)))
    (when (SCM_FALSEP hook)
      (SCM_BIND_PROC make-event-hook "make-event-hook" (SCM_MODULE Grv_GravitonEventModule))
      (set! hook (Scm_ApplyRec1 make-event-hook event))
      (set-global-hook! event hook))
    (return hook)))