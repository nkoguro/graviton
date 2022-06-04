;;;
;;; editor.scm - Simple text editor
;;;
;;;   Copyright (c) 2021 KOGURO, Naoki (naoki@koguro.net)
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
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use text.html-lite)

(define css '((style-rule body
                (background-color black)
                (margin 0))
              (style-rule (* (id container))
                (display flex)
                (flex-direction column)
                (height (100 vh))
                (font-family (:or Consolas SFMono-Regular "Roboto Mono" "Courier New" Courier monospace))
                (font-weight normal))
              (style-rule (* (id buffer))
                (flex #(1 1))
                (height 0)
                (overflow-y scroll)
                (background-color black)
                (color white)
                (padding #((2 px) (2 px) (2 px) (5 px))))
              (style-rule (* (id status))
                (height (1 em))
                (background-color white)
                (color black)
                (padding #((2 px) 0 (2 px) (5 px))))))

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))
    (with-window (grv-window
                   :css css
                   :body
                   (html:body
                    (html:div :id "container"
                              (html:grut-text :id "buffer")
                              (html:grut-text :id "status"))))
        (buffer status)
      (let ((filename #f)
            (status-keymap (make-keymap (global-keymap))))
        (define (update-title! modified?)
          (set! (~ document'title) (format "~a~a - Editor"
                                           (if modified? "*" "")
                                           (or filename "Untitled"))))
        (define (display-default-status-message)
          (display "C-/ to show menu" status))
        (define (update-status! ctx type str)
          (clear-screen status)
          (cond
            ((and (eq? type 'key)
                  (equal? str "C-/"))
             (display "C-/   f - load file, s - save file" status))
            (else
             (display-default-status-message))))
        (define (start-editor content-filename)
          (set! filename content-filename)
          (let1 content (cond
                          ((not filename)
                           "")
                          ((file-exists? filename)
                           (begin0
                               (file->string filename)
                             (format status "Read file - ~a" filename)))
                          (else
                           (begin0
                               ""
                             (format status "New file - ~a" filename))))
            (update-title! #f)
            (clear-screen buffer)
            (read-text/edit buffer
                            :input-continues #t
                            :initial-text content
                            :cursor-column 0
                            :cursor-row 0
                            :on-input update-status!
                            :on-change (lambda (ctx)
                                         (update-title! #t)))))

        (bind-key status-keymap "Escape" edit:cancel-edit)

        (bind-key (global-keymap) "C-/ f" (lambda (input-context)
                                            (let1 str (read-text/edit status :prompt "File: " :keymap status-keymap :initial-text "")
                                              (clear-screen status)
                                              (when str
                                                (edit:cancel-edit input-context)
                                                (start-editor str)))))

        (bind-key (global-keymap) "C-/ s" (lambda (input-context)
                                            (set! filename (or filename (read-text/edit status :prompt "File: " :keymap status-keymap :initial-text "")))
                                            (when filename
                                              (when (file-exists? filename)
                                                (move-file filename (path-swap-extension filename "bak")))
                                              (call-with-output-file filename
                                                (lambda (out)
                                                  (display (input-context-text-content input-context) out))))
                                            (update-title! #f)))

        (display-default-status-message)
        (start-editor #f)))))
