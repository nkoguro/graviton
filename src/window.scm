;;;
;;; window.scm - Window
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

(define-cproc window-center-point (gwin::<graviton-window>)
  ::<list>
  (return (SCM_LIST2 (Scm_MakeFlonum (/ (-> gwin logical-width) 2.0))
                     (Scm_MakeFlonum (/ (-> gwin logical-height) 2.0)))))

