;;;
;;; async.scm - Async
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

(define-module graviton.async
  (use data.queue)
  (use gauche.hook)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.threads)
  (use graviton.context)
  (use graviton.scheduler)
  (use srfi-42)
  (use util.match)

  (export <task-queue>
          <graviton-future>
          <graviton-channel>

          make-task-queue
          transform-future
          async-apply
          async
          await
          asleep
          task-yield
          make-channel
          channel-send
          channel-recv
          channel-recv/await
          channel-close

          submit-task

          set-future-values!

          current-task-queue
          main-task-queue

          task-start-hook
          task-end-hook))

(select-module graviton.async)

;;;

(define current-task-queue (make-parameter #f))

(define-class <task-queue> ()
  ((queue :init-form (make-mtqueue))
   (threads :init-value '())))

(define (task-worker task-queue)
  (let1 queue (slot-ref task-queue 'queue)
    (while (dequeue/wait! queue)
      => thunk
      (thunk))))

(define (make-task-queue n)
  (let* ((task-queue (make <task-queue>))
         (threads (list-ec (: i n)
                           (make-thread (cut task-worker task-queue)))))
    (slot-set! task-queue 'threads threads)
    (for-each thread-start! threads)
    task-queue))

(define task-start-hook (make-hook))
(define task-end-hook (make-hook))

(define thread-start-time (make-parameter #f))

(add-hook! task-start-hook (lambda ()
                             (thread-start-time (time->seconds (current-time)))))

(define (submit-task task-queue thunk)
  (let* ((app-context (application-context))
         (wrapped-thunk (lambda ()
                          (reset
                            (guard (e (else (report-error e)
                                            (exit 70)))
                              (parameterize ((application-context app-context)
                                             (current-task-queue task-queue))
                                (dynamic-wind
                                    (lambda ()
                                      (run-hook task-start-hook))
                                    thunk
                                    (lambda ()
                                      (run-hook task-end-hook)))))))))
    (enqueue! (slot-ref task-queue 'queue) wrapped-thunk)))

(define (submit-cont task-queue cont)
  (enqueue! (slot-ref task-queue 'queue)
            (lambda ()
              (reset
                (cont)))))

;;;

(define-application-context-slot main-task-queue (make-task-queue 1))

(define (main-task-queue)
  (application-context-slot-ref 'main-task-queue))

;;;

(define-class <graviton-future> ()
  ((lock :init-form (make-mutex))
   (values :init-value #f)
   (tqueue&continuations :init-value '())))

(define-class <graviton-future-transformer> ()
  ((lock :init-form (make-mutex))
   (original-future :init-keyword :original-future)
   (transformer :init-keyword :transformer)
   (cached-values :init-value #f)))

(define (transform-future future proc)
  (make <graviton-future-transformer> :original-future future :transformer proc))

(define (set-future-values! future vals)
  (with-locking-mutex (slot-ref future 'lock)
    (lambda ()
      (cond
        ((slot-ref future 'values)
         ;; If values is already set, do nothing.
         #f)
        (else
         (for-each (match-lambda ((task-queue cont)
                                  (submit-cont task-queue (cut cont vals))))
                   (slot-ref future 'tqueue&continuations))
         (slot-set! future 'values vals)
         (slot-set! future 'tqueue&continuations '()))))))

(define-method get-future-values ((future <graviton-future>) timeout timeout-vals)
  (let1 lock (slot-ref future 'lock)
    (mutex-lock! lock)
    (cond
      ((slot-ref future 'values)
       => (lambda (vals)
            (mutex-unlock! lock)
            vals))
      (else
       (let1 task-queue (current-task-queue)
         (shift cont
           (push! (slot-ref future 'tqueue&continuations) (list task-queue cont))
           (mutex-unlock! lock)
           (when timeout
             (add-timeout! timeout (lambda ()
                                     (set-future-values! future timeout-vals))))))))))

(define-method get-future-values ((future-transformer <graviton-future-transformer>) timeout timeout-vals)
  (with-locking-mutex (slot-ref future-transformer 'lock)
    (lambda ()
      (or (slot-ref future-transformer 'cached-values)
          (let1 vals (get-future-values (slot-ref future-transformer 'original-future) timeout timeout-vals)
            (slot-set! future-transformer 'cached-values
                       (and vals
                            (values->list (apply (slot-ref future-transformer 'transformer) vals))))
            (slot-ref future-transformer 'cached-values))))))

(define (await future :optional (timeout #f) :rest timeout-vals)
  (let1 vals (get-future-values future timeout timeout-vals)
    (if (null? vals)
        (undefined)
        (apply values vals))))

;; (define (asleep sec)
;;   (let1 future (make <graviton-future>)
;;     (await future sec)))

(define (asleep sec)
  (cond
    ((<= sec 0)
     #f)
    (else
     (let1 task-queue (current-task-queue)
       (shift cont
         (add-timeout! sec (lambda ()
                             (submit-cont task-queue cont))))))))

(define (task-yield :optional (time-slice 0))
  (when (<= time-slice (- (time->seconds (current-time)) (thread-start-time)))
    (asleep 0)))

(define (async-apply task-queue proc args)
  (let ((future (make <graviton-future>))
        (app-context (application-context)))
    (submit-task task-queue
                  (lambda ()
                    (set-future-values! future (values->list (apply proc args)))))
    future))

(define-syntax async
  (syntax-rules ()
    ((_ task-queue expr ...)
     (async-apply task-queue (lambda () expr ...) '()))))

;;;

(define-class <graviton-channel> ()
  ((lock :init-form (make-mutex))
   (queue :init-form (make-queue))
   (tqueue&continuation-queue :init-form (make-queue))
   (closed? :init-value #f)))

(define (make-channel)
  (make <graviton-channel>))

(define (channel-closed? channel)
  (with-locking-mutex (slot-ref channel 'lock)
    (lambda ()
      (slot-ref channel 'closed?))))

(define (channel-send channel obj)
  (with-locking-mutex (slot-ref channel 'lock)
    (lambda ()
      (when (slot-ref channel 'closed?)
        (errorf "channel ~s is already closed." channel))
      (let ((queue (slot-ref channel 'queue))
            (tqueue&continuation-queue (slot-ref channel 'tqueue&continuation-queue)))
        (cond
          ((queue-empty? tqueue&continuation-queue)
           (enqueue! queue obj))
          (else
           (match-let1 (task-queue cont) (dequeue! tqueue&continuation-queue)
             (submit-cont task-queue (cut cont obj)))))))))

(define (channel-send-timeout-val channel cont timeout-val)
  (define (cont? tqueue&cont)
    (match-let1 (_ k) tqueue&cont (eq? k cont)))
  (with-locking-mutex (slot-ref channel 'lock)
    (lambda ()
      (and-let* ((tqueue&continuation-queue (slot-ref channel 'tqueue&continuation-queue))
                 (tqueue&cont (find-in-queue cont? tqueue&continuation-queue)))
        (remove-from-queue! cont? tqueue&continuation-queue)
        (match-let1 (task-queue _) tqueue&cont
          (submit-cont task-queue (cut cont timeout-val)))))))

(define (channel-recv channel :optional (fallback #f))
  (with-locking-mutex (slot-ref channel 'lock)
    (lambda ()
      (dequeue! (slot-ref channel 'queue) fallback))))

(define (channel-recv/await channel :optional (timeout #f) (timeout-val #f))
  (let ((lock (slot-ref channel 'lock))
        (queue (slot-ref channel 'queue))
        (tqueue&continuation-queue (slot-ref channel 'tqueue&continuation-queue)))
    (mutex-lock! lock)
    (cond
      ((slot-ref channel 'closed?)
       (mutex-unlock! lock)
       (eof-object))
      ((queue-empty? queue)
       (let1 task-queue (current-task-queue)
         (shift cont
           (enqueue! tqueue&continuation-queue (list task-queue cont))
           (mutex-unlock! lock)
           (when timeout
             (add-timeout! timeout (lambda ()
                                     (channel-send-timeout-val channel cont timeout-val)))))))
      (else
       (begin0
         (dequeue! queue)
         (mutex-unlock! lock))))))

(define (channel-close channel)
  (with-locking-mutex (slot-ref channel 'lock)
    (lambda ()
      (dequeue-all! (slot-ref channel 'queue))
      (slot-set! channel 'closed? #t)
      (for-each (match-lambda
                  ((task-queue cont)
                   (submit-cont task-queue (cut cont (eof-object)))))
                (dequeue-all! (slot-ref channel 'tqueue&continuation-queue))))))

