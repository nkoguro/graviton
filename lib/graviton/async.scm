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
  (use control.thread-pool)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.threads)
  (use graviton.common)

  (export-all)
  ) ;; end of define-module

(select-module graviton.async)
(dynamic-load "graviton-async")

(define current-thread-pool (make-parameter #f))

(define (submit/thread thunk)
  (thread-start! (make-thread thunk)))

(define (submit/pool pool thunk)
  (add-job! pool (lambda ()
                   (parameterize ((current-thread-pool pool))
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

(define (async/pool-apply pool proc :rest args)
  (%async-apply (cut submit/pool pool <>) proc args))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...)))))

(define-syntax async/thread
  (syntax-rules ()
    ((_ expr ...)
     (async/thread-apply (lambda () expr ...)))))

(define-syntax async/pool
  (syntax-rules ()
    ((_ pool expr ...)
     (async/pool-apply pool (lambda () expr ...)))))

(define (await future)
  (cond
    ((is-a? future <graviton-future>)
     (receive (result exception) (cond
                                   ((in-event-loop?)
                                    (shift cont
                                      (add-future-continuation! future cont)))
                                   ((current-thread-pool)
                                    (shift cont
                                      (add-future-continuation! future (let1 pool (current-thread-pool)
                                                                         (lambda args
                                                                           (submit/pool pool (lambda ()
                                                                                               (apply cont args))))))))
                                   (else
                                    (future-result&exception future)))
       (cond
         (result
          (apply values result))
         (exception
          (raise exception))
         (else
          (error "[BUG] neither result nor exception is specified.")))))
    (else
     future)))

(define (await-sleep sec)
  (cond
    ((in-event-loop?)
     (shift cont
       (add-cont-timer! sec cont)))
    ((current-thread-pool)
     (shift cont
       (add-cont-timer! sec (let1 pool (current-thread-pool)
                              (lambda ()
                                (submit/pool pool cont))))))
    (else
     (thread-sleep! sec))))

(define (yield)
  (cond
    ((in-event-loop?)
     (shift cont (submit/main cont)))
    ((current-thread-pool)
     (shift cont (submit/pool (current-thread-pool) cont)))
    (else
     #f)))
