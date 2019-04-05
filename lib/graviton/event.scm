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

(define-module graviton.event
  (use graviton.audio)
  (use graviton.common)
  (use graviton.video)

  (export-all))

(select-module graviton.event)
(dynamic-load "graviton-event")

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


