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
(use gauche.hook)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use text.html-lite)

(bind-url-path "/editor.css" (build-path (sys-dirname (current-load-path)) "editor.css"))

(grv-window
  :path "/"
  :css "/editor.css"
  :body
  (html:body
   (html:div :id "container"
             (html:grv-text :id "buffer")
             (html:grv-text :id "status")))

  (let-elements (buffer status)
    (enable-screen-edit buffer)
    (let ((filename #f)
          (content-version (~ buffer'content-version)))

      (buffer'bind-key "C-/ f" (lambda ()
                                 (status'show-cursor)
                                 (clear-input-buffer! status)
                                 (let1 str (read-string-from-grv-text status :prompt "File: ")
                                   (status'hide-cursor)
                                   (status'remove-all-lines)
                                   (when str
                                     (set! filename str)
                                     (buffer'remove-all-lines)
                                     (cond
                                       ((file-exists? filename)
                                        (buffer'insert-text (file->string filename))
                                        (status'print-text (format "Read file - ~a" filename)))
                                       (else
                                        (status'print-text (format "New file - ~a" filename))))
                                     (set! content-version (~ buffer'content-version))))
                                 (buffer'focus)))
      (add-hook! (~ buffer'pre-key-handler-hook) (lambda (key)
                                                   (status'remove-all-lines)
                                                   (cond
                                                     ((equal? key "C-/")
                                                      (status'print-text "C-/"))
                                                     (else
                                                      #t))))
      (add-hook! (~ buffer'pre-input-handler-hook) (lambda (str)
                                                     (status'remove-all-lines)))
      (buffer'focus)

      (on-idle ()
        (set! (~ document'title) (format "~a~a - Editor"
                                         (if (= content-version (~ buffer'content-version))
                                           ""
                                           "*")
                                         (or filename "Untitled")))))

    (worker-thread-idle-timeout 0.1)))

(define (main args)
  (let-args (cdr args)
      ((browser? "browser" #f))

    (if browser?
      (grv-start-server)
      (grv-start-player :resizable? #t))))
