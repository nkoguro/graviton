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
  (use gauche.hook)
  (use graviton.audio)
  (use graviton.common)
  (use graviton.video)

  (export-all))

(select-module graviton.event)
(dynamic-load "graviton-event")

(define hook-arity-table
  '(
    ;; Window hook
    (window-shown . 1)
    (window-hidden . 1)
    (window-exposed . 1)
    (window-moved . 3)
    (window-resized . 3)
    (window-size-changed . 3)
    (window-minimized . 1)
    (window-maximized . 1)
    (window-restored . 1)
    (window-enter . 1)
    (window-leave . 1)
    (window-focus-gained . 1)
    (window-focus-lost . 1)
    (window-close . 1)
    (window-take-focus . 1)
    (window-hit-test . 1)
    (window-update-begin . 1)
    (window-update-complete . 1)
    (key-down . 5)
    (key-up . 5)
    (text-editing . 4)
    (text-input . 2)
    (mouse-motion . 7)
    (mouse-button-down . 6)
    (mouse-button-up . 6)
    (mouse-wheel . 5)
    (drop-file . 2)
    (drop-text . 2)
    (drop-begin . 1)
    (drop-complete . 1)

    ;; Global hook
    (joystick-axis-motion . 3)
    (joystick-ball-motion . 4)
    (joystick-hat-motion . 3)
    (joystick-button-down . 3)
    (joystick-button-up . 3)
    (joystick-device-added . 1)
    (joystick-device-removed . 1)
    (controller-axis-motion . 3)
    (controller-button-down . 3)
    (controller-button-up . 3)
    (controller-device-added . 1)
    (controller-device-removed . 1)
    (controller-device-remapped . 1)
    (audio-device-added . 2)
    (audio-device-removed . 2)
    (quit . 0)
    (finger-motion . 7)
    (finger-down . 7)
    (finger-up . 7)
    (multi-gesture . 6)
    (dollar-gesture . 6)
    (dollar-record . 6)
  ))

(define (make-event-hook name)
  (let1 hook-arity (or (assoc-ref hook-arity-table name #f)
                       (errorf "Invalid hook name: ~a" name))
    (make-hook hook-arity)))

(define (window-shown-hook-of win)
  (window-hook win 'window-shown))

(define (window-hidden-hook-of win)
  (window-hook win 'window-hidden))

(define (window-exposed-hook-of win)
  (window-hook win 'window-exposed))

(define (window-moved-hook-of win)
  (window-hook win 'window-moved))

(define (window-resized-hook-of win)
  (window-hook win 'window-resized))

(define (window-size-changed-hook-of win)
  (window-hook win 'window-size-changed))

(define (window-minimized-hook-of win)
  (window-hook win 'window-minimized))

(define (window-maximized-hook-of win)
  (window-hook win 'window-maximized))

(define (window-restored-hook-of win)
  (window-hook win 'window-restored))

(define (window-enter-hook-of win)
  (window-hook win 'window-enter))

(define (window-leave-hook-of win)
  (window-hook win 'window-leave))

(define (window-focus-gained-hook-of win)
  (window-hook win 'window-focus-gained))

(define (window-focus-lost-hook-of win)
  (window-hook win 'window-focus-lost))

(define (window-close-hook-of win)
  (window-hook win 'window-close))

(define (window-take-focus-hook-of win)
  (window-hook win 'window-take-focus))

(define (window-hit-test-hook-of win)
  (window-hook win 'window-hit-test))

(define (window-update-begin-hook-of win)
  (window-hook win 'window-update-begin))

(define (window-update-complete-hook-of win)
  (window-hook win 'window-update-complete))

(define (key-down-hook-of win)
  (window-hook win 'key-down))

(define (key-up-hook-of win)
  (window-hook win 'key-up))

(define (text-editing-hook-of win)
  (window-hook win 'text-editing))

(define (text-input-hook-of win)
  (window-hook win 'text-input))

(define (mouse-motion-hook-of win)
  (window-hook win 'mouse-motion))

(define (mouse-button-down-hook-of win)
  (window-hook win 'mouse-button-down))

(define (mouse-button-up-hook-of win)
  (window-hook win 'mouse-button-up))

(define (mouse-wheel-hook-of win)
  (window-hook win 'mouse-wheel))

(define (drop-file-hook-of win)
  (window-hook win 'drop-file))

(define (drop-text-hook-of win)
  (window-hook win 'drop-text))

(define (drop-begin-hook-of win)
  (window-hook win 'drop-begin))

(define (drop-complete-hook-of win)
  (window-hook win 'drop-complete))

(define joystick-axis-motion-hook (global-hook 'joystick-axis-motion))

(define joystick-ball-motion-hook (global-hook 'joystick-ball-motion))

(define joystick-hat-motion-hook (global-hook 'joystick-hat-motion))

(define joystick-button-down-hook (global-hook 'joystick-button-down))

(define joystick-button-up-hook (global-hook 'joystick-button-up))

(define joystick-device-added-hook (global-hook 'joystick-device-added))

(define joystick-device-removed-hook (global-hook 'joystick-device-removed))

(define controller-axis-motion-hook (global-hook 'controller-axis-motion))

(define controller-button-down-hook (global-hook 'controller-button-down))

(define controller-button-up-hook (global-hook 'controller-button-up))

(define controller-device-added-hook (global-hook 'controller-device-added))

(define controller-device-removed-hook (global-hook 'controller-device-removed))

(define controller-device-remapped-hook (global-hook 'controller-device-remapped))

(define audio-device-added-hook (global-hook 'audio-device-added))

(define audio-device-removed-hook (global-hook 'audio-device-removed))

(define quit-hook (global-hook 'quit))

(define finger-motion-hook (global-hook 'finger-motion))

(define finger-down-hook (global-hook 'finger-down))

(define finger-up-hook (global-hook 'finger-up))

(define multi-gesture-hook (global-hook 'multi-gesture))

(define dollar-gesture-hook (global-hook 'dollar-gesture))

(define dollar-record-hook (global-hook 'dollar-record))

(define (update-all-windows)
  (let1 win-list (all-windows)
    (for-each (lambda (win)
                ((window-update-begin-hook-of win) win))
              win-list)
    (update-window-contents win-list)
    (for-each (lambda (win)
                ((window-update-complete-hook-of win) win))
              win-list)))
