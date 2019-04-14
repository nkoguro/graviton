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
  (use graviton.audio)
  (use graviton.color)
  (use graviton.common)
  (use graviton.event)
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
          async/pool
          async-apply
          async/thread-apply
          async/pool-apply
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

          window-hook
          window-shown-hook-of
          window-hidden-hook-of
          window-exposed-hook-of
          window-moved-hook-of
          window-resized-hook-of
          window-size-changed-hook-of
          window-minimized-hook-of
          window-maximized-hook-of
          window-restored-hook-of
          window-enter-hook-of
          window-leave-hook-of
          window-focus-gained-hook-of
          window-focus-lost-hook-of
          window-close-hook-of
          window-take-focus-hook-of
          window-hit-test-hook-of
          window-update-begin-hook-of
          window-update-complete-hook-of
          key-down-hook-of
          key-up-hook-of
          text-editing-hook-of
          text-input-hook-of
          mouse-motion-hook-of
          mouse-button-down-hook-of
          mouse-button-up-hook-of
          mouse-wheel-hook-of
          drop-file-hook-of
          drop-text-hook-of
          drop-begin-hook-of
          drop-complete-hook-of

          global-hook
          joystick-axis-motion-hook
          joystick-ball-motion-hook
          joystick-hat-motion-hook
          joystick-button-down-hook
          joystick-button-up-hook
          joystick-device-added-hook
          joystick-device-removed-hook
          controller-axis-motion-hook
          controller-button-down-hook
          controller-button-up-hook
          controller-device-added-hook
          controller-device-removed-hook
          controller-device-remapped-hook
          audio-device-added-hook
          audio-device-removed-hook
          quit-hook
          finger-motion-hook
          finger-down-hook
          finger-up-hook
          multi-gesture-hook
          dollar-gesture-hook
          dollar-record-hook

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
(dynamic-load "graviton")

(add-hook! make-window-hook
  (lambda (win)
    (add-hook! (window-close-hook-of win) destroy-window)
    (add-hook! (window-resized-hook-of win) reflect-resized-window-parameter)))

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
      (add-hook! (key-up-hook-of win)
        (lambda (win scancode sym mod repeat?)
          (case scancode
            ((escape)
             (destroy-window win))
            ((f)
             (set-window-fullscreen! win (not (window-fullscreen? win)))))))
      (add-hook! (text-input-hook-of win)
        (lambda (win text)
          (cond
            ((equal? text "+")
             (let ((w (* (window-physical-width win) 2))
                   (h (* (window-physical-height win) 2)))
               (set-window-physical-size! win w h)))
            ((equal? text "-")
             (let ((w (/ (window-physical-width win) 2))
                   (h (/ (window-physical-height win) 2)))
               (when (and (<= (image-width image) w) (<= (image-height image) h))
                 (set-window-physical-size! win w h))))))))))


;;;
;;; REPL
;;;

(define (grv-repl)
  (define prompter
    (let1 user-module (find-module 'user)
      (lambda ()
        (let1 m ((with-module gauche.internal vm-current-module))
          (if (eq? m user-module)
              (display "graviton> ")
              (format #t "graviton[~a]> " (module-name m)))
          (flush)))))

  (define (reader)
    (cond
      ((event-loop-running?)
       (await (async/thread
                (with-module gauche.interactive
                  (%reader)))))
      (else
       (eof-object))))

  (grv-main
    (lambda ()
      (read-eval-print-loop
        reader
        (with-module gauche.interactive
          %evaluator)
        (with-module gauche.interactive
          %printer)
        prompter))))
