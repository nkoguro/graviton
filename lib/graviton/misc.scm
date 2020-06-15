;;;
;;; misc.scm - Misc utilities
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

(define-module graviton.misc
  (use gauche.logger)
  (use gauche.threads)

  (export make-id-generator

          grv-log-config
          access-log-drain
          error-log-drain
          log-level
          log-debug
          log-info
          log-error))

(select-module graviton.misc)

;;;

(define (make-id-generator :optional (max-value #f) (start 0))
  (let1 counter-atom (atom start)
    (lambda ()
      (atomic-update! counter-atom (lambda (x)
                                     (if max-value
                                         (modulo (+ x 1) max-value)
                                         (+ x 1)))))))

;;;

(define-class <log-config> ()
  ((access-log-drain :init-value #f)
   (error-log-drain :init-value #t)
   (log-level :init-value 1)))

(define *log-config* (make <log-config>))

(define (grv-log-config :key
                        access-log-drain
                        error-log-drain
                        log-level)
  (set! *log-config* (config-with-params <log-config> access-log-drain error-log-drain log-level)))

(define (access-log-drain)
  (slot-ref *log-config* 'access-log-drain))

(define (error-log-drain)
  (slot-ref *log-config* 'error-log-drain))

(define (log-level)
  (slot-ref *log-config* 'log-level))

(define (log-debug fmt :rest args)
  (when (<= (log-level) 0)
    (apply log-format fmt args)))

(define (log-info fmt :rest args)
  (when (<= (log-level) 1)
    (apply log-format fmt args)))

(define (log-error fmt :rest args)
  (when (<= (log-level) 2)
    (apply log-format fmt args)))
