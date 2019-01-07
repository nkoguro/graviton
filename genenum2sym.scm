;;;
;;; genenum2sym.scm - Enum-to-symbol functions generator
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

(use file.util)
(use gauche.process)
(use scheme.list)
(use srfi-13)

(define (extract-enums filename re ignore-enums prefix)
  (reverse (remove (lambda (pair)
                     (member (car pair) ignore-enums))
                   (call-with-input-file filename
                     (lambda (in)
                       (port-fold (lambda (line enum-alist)
                                    (rxmatch-case line
                                      (re (_ enum id)
                                       (alist-cons enum
                                                   (string->symbol
                                                     (string-append prefix
                                                                    (string-downcase (regexp-replace-all #/_/ id "-"))))
                                                   enum-alist))
                                      (else
                                       enum-alist)))
                                  '()
                                  (cut read-line in)))))))

(define (extract-states dir)
  (extract-enums (build-path dir "SDL_events.h")
                 #/^(?:\s+|#define\s+)(SDL_(RELEASED|PRESSED))/
                 '()
                 ""))

(define (extract-scancodes dir)
  (extract-enums (build-path dir "SDL_scancode.h")
                 #/^\s+(SDL_SCANCODE_([0-9A-Za-z_]+))/
                 '("SDL_SCANCODE_UNKNOWN")
                 ""))

(define (extract-keycodes dir)
  (extract-enums (build-path dir "SDL_keycode.h")
                 #/^\s+(SDLK_([0-9A-Za-z_]+))/
                 '("SDLK_UNKNOWN")
                 ""))

(define (extract-mods dir)
  (extract-enums (build-path dir "SDL_keycode.h")
                 #/^(?:\s+|#define\s+)(KMOD_([0-9A-Za-z_]+))/
                 '("KMOD_NONE" "KMOD_RESERVED")
                 ""))

(define (extract-window-events dir)
  (extract-enums (build-path dir "SDL_video.h")
                 #/^\s+(SDL_WINDOWEVENT_([0-9A-Za-z_]+))/
                 '("SDL_WINDOWEVENT_NONE")
                 "window-"))

(define (extract-axis dir)
  (extract-enums (build-path dir "SDL_gamecontroller.h")
                 #/^(?:\s+|#define\s+)(SDL_CONTROLLER_AXIS_([0-9A-Za-z_]+))/
                 '("SDL_CONTROLLER_AXIS_INVALID" "SDL_CONTROLLER_AXIS_MAX")
                 ""))

(define (extract-buttons dir)
  (extract-enums (build-path dir "SDL_gamecontroller.h")
                 #/^(?:\s+|#define\s+)(SDL_CONTROLLER_BUTTON_([0-9A-Za-z_]+))/
                 '("SDL_CONTROLLER_BUTTON_INVALID" "SDL_CONTROLLER_BUTTON_MAX")
                 ""))

(define (extract-mouse-button-masks dir)
  (let1 rename-alist '((l . left)
                       (m . middle)
                       (r . right))
    (map (lambda (pair)
           (cons (car pair) (assoc-ref rename-alist (cdr pair) (cdr pair))))
         (extract-enums (build-path dir "SDL_mouse.h")
                        #/^(?:\s+|#define\s+)(SDL_BUTTON_([0-9A-Za-z_]+)MASK)/
                        '()
                        ""))))

(define (extract-mouse-buttons dir)
  (remove (lambda (pair)
            (#/.*MASK$/ (car pair)))
          (extract-enums (build-path dir "SDL_mouse.h")
                         #/^(?:\s+|#define\s+)(SDL_BUTTON_([0-9A-Za-z_]+))/
                         '()
                         "")))

(define (extract-hat-positions dir)
  (extract-enums (build-path dir "SDL_joystick.h")
                 #/^(?:\s+|#define\s+)(SDL_HAT_([0-9A-Za-z_]+))/
                 '()
                 ""))

(define (detect-sdl-header-directory)
  (let loop ((dirs (string-split (regexp-replace "^-I"
                                                 (process-output->string '("pkg-config" "--cflags-only-I" "sdl2"))
                                                 "")
                                 ":")))
    (cond
      ((null? dirs)
       (error "Can't find libsdl header directory."))
      ((file-exists? (build-path (car dirs) "SDL.h"))
       (car dirs))
      (else
       (loop (cdr dirs))))))

(define (generate-enum->symbol-function function-name input-type alist fallback-symbol)
  `(define-cfn ,function-name (v :: ,input-type)
     ::ScmObj :static
     (case v
       ,@(map (lambda (pair)
                `((,(string->symbol (car pair))) (return ',(cdr pair))))
              alist)
       (else
        (return ',fallback-symbol)))))

(define (generate-mask->symbols-function function-name input-type alist)
  `(define-cfn ,function-name (v :: ,input-type)
     ::ScmObj :static
     (let* ((syms SCM_NIL))
       ,@(map (lambda (pair)
                `(when (logand v ,(string->symbol (car pair)))
                   (set! syms (Scm_Cons ',(cdr pair) syms))))
              alist)
       (return syms))))

(define (generate-code out sdl-header-dir)
  (let ((state-alist (extract-states sdl-header-dir))
        (scancode-alist (extract-scancodes sdl-header-dir))
        (keycode-alist (extract-keycodes sdl-header-dir))
        (mod-alist (extract-mods sdl-header-dir))
        (window-event-alist (extract-window-events sdl-header-dir))
        (axis-alist (extract-axis sdl-header-dir))
        (button-alist (extract-buttons sdl-header-dir))
        (mouse-button-mask-alist (extract-mouse-button-masks sdl-header-dir))
        (mouse-button-alist (extract-mouse-buttons sdl-header-dir))
        (hat-position-alist (extract-hat-positions sdl-header-dir)))
    (display ";;;; Generated automatically from SDL header files.\n" out)
    (pprint `(inline-stub
               ,(generate-enum->symbol-function 'state->symbol 'Uint8 state-alist 'unknown)
               ,(generate-enum->symbol-function 'scancode->symbol 'SDL_Scancode scancode-alist 'unknown)
               ,(generate-enum->symbol-function 'keycode->symbol 'SDL_Keycode keycode-alist 'unknown)
               ,(generate-mask->symbols-function 'kmod->symbols 'Uint16 mod-alist)
               ,(generate-enum->symbol-function 'window-event->symbol 'SDL_WindowEventID window-event-alist 'window-unknown-event)
               ,(generate-enum->symbol-function 'axis->symbol 'SDL_GameControllerAxis axis-alist 'unknown)
               ,(generate-enum->symbol-function 'button->symbol 'SDL_GameControllerButton button-alist 'unknown)
               ,(generate-mask->symbols-function 'mouse-button-state->symbols 'Uint32 mouse-button-mask-alist)
               ,(generate-enum->symbol-function 'mouse-button->symbol 'Uint8 mouse-button-alist 'unknown)
               ,(generate-enum->symbol-function 'hat-position->symbol 'Uint8 hat-position-alist 'unknown))
            :port out)))

(define (main args)
  (generate-code (current-output-port) (detect-sdl-header-directory))
  0)