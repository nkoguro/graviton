;;;
;;; gtk.scm - Dialog implementation with zenity (GTK)
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

(define-module graviton.dialog.gtk
  (use gauche.process)
  (use graviton.async)
  (use graviton.video)
  (use util.match)

  (export-all)
  )

(select-module graviton.dialog.gtk)

(define (zenity win dialog-option param-alist cont)
  (let* ((wid (window-system-id win))
         (process (run-process `("zenity" "--modal" "--attach" ,wid ,dialog-option
                                 ,@(fold (lambda (pair args)
                                           (append args (cond
                                                          ((cdr pair)
                                                           (list (format "--~a" (car pair)) (cdr pair)))
                                                          (else
                                                           '()))))
                                         '()
                                         param-alist))
                               :output 'stdout
                               :error :null)))
    (async/thread
     (let1 output (call-with-output-string
                    (cut copy-port (process-output process) <>))
       (process-wait process)
       (let1 exit-code (sys-wait-exit-status (process-exit-status process))
         (cont exit-code output))))))

(define (show-gtk-info-dialog win :key (title #f) (text #f) (ok-label #f))
  (zenity win "--info" `((title . ,title) (text . ,text) (ok-label . ,ok-label))
          (lambda (exit-code output)
            (case exit-code
              ((0)
               'ok)
              (else
               'cancel)))))

(define (show-gtk-warning-dialog win :key (title #f) (text #f) (ok-label #f))
  (zenity win "--warning" `((title . ,title) (text . ,text) (ok-label . ,ok-label))
          (lambda (exit-code output)
            (case exit-code
              ((0)
               'ok)
              (else
               'cancel)))))

(define (show-gtk-error-dialog win :key (title #f) (text #f) (ok-label #f))
  (zenity win "--error" `((title . ,title) (text . ,text) (ok-label . ,ok-label))
          (lambda (exit-code output)
            (case exit-code
              ((0)
               'ok)
              (else
               'cancel)))))
