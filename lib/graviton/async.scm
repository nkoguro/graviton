;;;
;;; async.scm - Async
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

(define-module graviton.async
  (use gauche.threads)
  (use graviton.common)

  (export make-future
          set-future-result!
          set-future-exception!
          async
          async/thread
          await
          await-sleep
          yield
          )
  ) ;; end of define-module

(select-module graviton.async)
(dynamic-load "graviton-async")

(define (submit/thread thunk)
  (thread-start! (make-thread
                   (lambda ()
                     (thunk)))))

(define (%async-apply submit proc args)
  (let ((args (apply list args))
        (future (make-future)))
    (submit (lambda ()
              (reset
                (guard (e (else (set-future-exception! future e (report-error e #f))))
                  (receive result (apply proc args)
                    (set-future-result! future result))))))
    future))

(define (async-apply proc :rest args)
  (%async-apply submit/main proc args))

(define (async/thread-apply proc :rest args)
  (%async-apply submit/thread proc args))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...)))))

(define-syntax async/thread
  (syntax-rules ()
    ((_ expr ...)
     (async/thread-apply (lambda () expr ...)))))

(define (await future)
  (unless (on-main-thread?)
    (error "await is unavailable on non-main thread."))
  (cond
    ((is-a? future <graviton-future>)
     (receive (result exception) (shift cont
                                   (add-future-continuation! future cont))
       (cond
         (result
          (apply values result))
         (exception
          (raise exception))
         (else
          (error "[BUG] result and exception aren't specified.")))))
    (else
     future)))

(define (await-sleep sec)
  (unless (on-main-thread?)
    (error "await-sleep is unavailable on non-main thread."))
  (shift cont
    (add-cont-timer! sec (lambda ()
                           (submit/main cont)))))

(define (yield)
  (unless (on-main-thread?)
    (error "yield is unavailable on non-main thread."))
  (shift cont (submit/main cont)))
