;;;
;;; grut.scm - Graviton Utility Toolkit
;;;
;;;   Copyright (c) 2020 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.grut
  (use graviton)
  (use graviton.app)
  (use graviton.jsffi)
  (use graviton.misc)
  (use text.html-lite)
  (use util.list)
  (use util.match)

  (extend graviton.grut.audio
          graviton.grut.clipboard
          graviton.grut.text)

  (export load-image
          load-audio

          copy-text-to-clipboard))

(select-module graviton.grut)

(import-js ("/_g/grut/grut.mjs" :as Grut))
(autoload-css "/_g/grut/grut.css")

;;;

(define (load-image url :key (content-type #f) (on-error :error))
  (or (jslet/result ((url::string))
        (let ((img (make Image)))
          (set! img.src url)
          (set! img.onload (lambda ()
                             (set! img.onload undefined)
                             (set! img.onerror undefined)
                             (result img)))
          (set! img.onerror (lambda ()
                              (set! img.onload undefined)
                              (set! img.onerror undefined)
                              (result #f)))))
      (case on-error
        ((#f) #f)
        ((:error) (errorf "Failed to load image: ~a" url))
        (else
         (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error)))))

(define (load-audio url :key (content-type #f) (on-error :error))
  (receive (audio err) (jslet/result ((url::string))
                         (let1 audio (make Audio url)
                           (set! audio.onloadeddata (lambda ()
                                                      (set! audio.onloadeddata undefined)
                                                      (set! audio.onerror undefined)
                                                      (result audio #f)))
                           (set! audio.onerror (lambda ()
                                                 (set! audio.onloadeddata undefined)
                                                 (set! audio.onerror undefined)
                                                 (result #f audio.error.message)))))
    (or audio
        (case on-error
          ((#f) #f)
          ((:error) (errorf "Failed to load audio: ~a (~a)" url err))
          (else
           (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error))))))


