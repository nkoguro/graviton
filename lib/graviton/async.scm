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
  (use graviton.app)
  (use graviton.scheduler)
  (use srfi-42)
  (use util.match)

  (export <task-queue>
          <graviton-future>
          <graviton-channel>

          make-task-queue
          task-queue-min-num-threads
          task-queue-min-num-threads-set!
          task-queue-max-num-threads
          task-queue-max-num-threads-set!
          task-queue-worker-timeout
          task-queue-worker-timeout-set!

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
          async-task-queue
          default-async-task-queue

          task-start-hook
          task-end-hook))

(select-module graviton.async)

;;;

(define current-task-queue (make-parameter #f))
(define async-task-queue (make-parameter #f))

(define-application-context-slot task-queue-list '())

(define-class <task-queue> ()
  ((lock :init-form (make-mutex))
   (state :init-value 'active)
   (min-num-threads :init-keyword :min-num-threads)
   (max-num-threads :init-keyword :max-num-threads)
   (worker-timeout :init-keyword :worker-timeout)
   (name :init-keyword :name)
   (working-thread-table :init-form (make-hash-table 'eq?))
   (queue :init-form (make-mtqueue))
   (threads :init-value '())))

(define-method write-object ((tq <task-queue>) port)
  (if-let1 name (slot-ref tq 'name)
           (format port "#<task-queue:~a>" name)
           (display "#<task-queue>" port)))

(define (task-queue-lock-recursively! tq)
  (let1 mutex (slot-ref tq 'lock)
    (if (eq? (mutex-state mutex) (current-thread))
        (let1 n (mutex-specific mutex)
          (mutex-specific-set! mutex (+ n 1)))
        (begin
          (mutex-lock! mutex)
          (mutex-specific-set! mutex 0)))))

(define (task-queue-unlock-recursively! tq)
  (let* ((mutex (slot-ref tq 'lock))
         (n (mutex-specific mutex)))
    (if (= n 0)
        (mutex-unlock! mutex)
        (mutex-specific-set! mutex (- n 1)))))

(define (with-task-queue-lock tq thunk)
  (dynamic-wind
      (lambda ()
        (task-queue-lock-recursively! tq))
      thunk
      (lambda ()
        (task-queue-unlock-recursively! tq))))

(define (task-queue-safe-slot-ref tq slot)
  (with-task-queue-lock tq
    (lambda ()
      (slot-ref tq slot))))

(define (task-queue-min-num-threads tq)
  (task-queue-safe-slot-ref tq 'min-num-threads))

(define (task-queue-min-num-threads-set! tq min-num-threads)
  (with-task-queue-lock tq
    (lambda ()
      (slot-set! tq 'min-num-threads min-num-threads))))

(define (task-queue-max-num-threads tq)
  (task-queue-safe-slot-ref tq 'max-num-threads))

(define (task-queue-max-num-threads-set! tq max-num-threads)
  (with-task-queue-lock tq
    (lambda ()
      (slot-set! tq 'max-num-threads max-num-threads))))

(define (task-queue-worker-timeout tq)
  (task-queue-safe-slot-ref tq 'worker-timeout))

(define (task-queue-worker-timeout-set! tq worker-timeout)
  (with-task-queue-lock tq
    (lambda ()
      (slot-set! tq 'worker-timeout worker-timeout))))

(define (task-worker-mark-idle tq)
  (with-task-queue-lock tq
    (lambda ()
      (hash-table-delete! (slot-ref tq 'working-thread-table) (current-thread)))))

(define (task-worker-mark-busy tq)
  (with-task-queue-lock tq
    (lambda ()
      (hash-table-put! (slot-ref tq 'working-thread-table) (current-thread) #t))))

(define (task-worker-register! tq)
  (with-task-queue-lock tq
    (lambda ()
      (push! (slot-ref tq 'threads) (current-thread)))))

(define (task-worker-unregister! tq)
  (with-task-queue-lock tq
      (lambda ()
        (slot-set! tq 'threads (remove (cut eq? (current-thread) <>) (slot-ref tq 'threads))))))

(define (task-worker tq)
  (task-worker-register! tq)

  (let1 queue (task-queue-safe-slot-ref tq 'queue)
    (while (with-task-queue-lock tq
             (lambda ()
               (and (<= (slot-ref tq 'min-num-threads) (length (slot-ref tq 'threads)))
                    (eq? (slot-ref tq 'state) 'active))))
      (while (begin
               (task-worker-mark-idle tq)
               (dequeue/wait! queue (task-queue-safe-slot-ref tq 'worker-timeout) #f))
        => thunk
        (task-worker-mark-busy tq)
        (reset
          (thunk))))

    (task-worker-unregister! tq)
    #t))

(define (start-task-worker tq)
  (with-task-queue-lock tq
    (lambda ()
      (let* ((worker-id (+ (length (slot-ref tq 'threads)) 1))
             (name (and-let1 prefix (slot-ref tq 'name)
                     (format #f "~a:~d-~d" prefix (application-context-id) worker-id))))
        (thread-start! (make-thread (cut task-worker tq) name))))))


(define (make-task-queue min-num-threads :optional max-num-threads (worker-timeout #f) (name #f))
  (let* ((max-num-threads (if (undefined? max-num-threads)
                              min-num-threads
                              max-num-threads))
         (tq (make <task-queue>
               :min-num-threads min-num-threads
               :max-num-threads max-num-threads
               :worker-timeout worker-timeout
               :name name)))
    (dotimes (i min-num-threads)
      (start-task-worker tq))
    (application-context-slot-atomic-update! 'task-queue-list
      (lambda (lst)
        (cons tq lst)))
    tq))

(define task-start-hook (make-hook))
(define task-end-hook (make-hook))

(define thread-start-time (make-parameter #f))

(add-hook! task-start-hook (lambda ()
                             (thread-start-time (time->seconds (current-time)))))

(define (task-queue-busy? tq)
  (with-task-queue-lock tq
    (lambda ()
      (let* ((tbl (slot-ref tq 'working-thread-table))
             (threads (slot-ref tq 'threads))
             (num-busy-worker (fold (lambda (thread i)
                                      (if (hash-table-get tbl thread #f)
                                          (+ i 1)
                                          i))
                                    0
                                    threads)))
        (= (length threads) num-busy-worker)))))

(define (task-queue-expandable? tq)
  (with-task-queue-lock tq
    (lambda ()
      (< (length (slot-ref tq 'threads)) (slot-ref tq 'max-num-threads)))))

(define (%submit tq thunk)
  (with-task-queue-lock tq
    (lambda ()
      (when (eq? (slot-ref tq 'state) 'active)
        (when (and (task-queue-busy? tq)
                   (task-queue-expandable? tq))
          (start-task-worker tq))
        (enqueue! (slot-ref tq 'queue) thunk)))))

(define (submit-task task-queue thunk)
  (let* ((app-context (application-context))
         (wrapped-thunk (lambda ()
                          ;; These parameters will not be changed in this thread.
                          (application-context app-context)
                          (current-task-queue task-queue)
                          ;; async-task-queue can be changed in thunk. So it needs to be restored when the continuation is called.
                          (parameterize ((async-task-queue (default-async-task-queue)))
                            (guard (e (else (report-error e)
                                            (app-exit 70)))
                              (dynamic-wind
                                  (lambda ()
                                    (run-hook task-start-hook))
                                  thunk
                                  (lambda ()
                                    (run-hook task-end-hook)))))
                          (undefined))))
    (%submit task-queue wrapped-thunk)))

(define (submit-cont task-queue cont)
  (%submit task-queue cont))

(define (task-queue-close! task-queue)
  (with-task-queue-lock task-queue
    (lambda ()
      (slot-set! task-queue 'state 'inactive)
      (let1 queue (slot-ref task-queue 'queue)
        (dequeue-all! queue)
        (dotimes (_ (length (slot-ref task-queue 'threads)))
          (enqueue! queue #f))))))

(define (task-queue-join! task-queue :optional (timeout #f))
  (with-task-queue-lock task-queue
    (lambda ()
      (when (eq? (slot-ref task-queue 'state) 'active)
        (errorf "task-queue ~s is still active" task-queue))))

  (for-each (lambda (thread)
              (unless (thread-join! thread timeout #f)
                (thread-terminate! thread)))
            (slot-ref task-queue 'threads))
  (slot-set! task-queue 'threads '()))

(define (all-task-queue-shutdown!)
  (let1 tq-list (application-context-slot-ref 'task-queue-list)
    (for-each (lambda (tq)
                (task-queue-close! tq))
              tq-list)
    (for-each (lambda (tq)
                (task-queue-join! tq))
              tq-list)))

(add-hook! app-close-hook all-task-queue-shutdown!)

;;;

(define-application-context-slot main-task-queue #f)
(define-application-context-slot default-async-task-queue #f)

(define (init-task-queue)
  (application-context-slot-set! 'main-task-queue (make-task-queue 1 1 #f "main-task-queue"))
  (application-context-slot-set! 'default-async-task-queue (make-task-queue 0 2 #f "default-async-task-queue")))

(add-hook! app-start-hook init-task-queue)

(define (main-task-queue)
  (application-context-slot-ref 'main-task-queue))

(define (default-async-task-queue)
  (application-context-slot-ref 'default-async-task-queue))

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

(define (asleep sec)
  (let1 task-queue (current-task-queue)
    (shift cont
      (add-timeout! sec (lambda ()
                          (submit-cont task-queue cont))))))

(define (task-yield :optional (time-slice 0))
  (when (<= time-slice (- (time->seconds (current-time)) (thread-start-time)))
    (asleep 0)))

(define (async-apply proc args)
  (let ((future (make <graviton-future>))
        (app-context (application-context)))
    (submit-task (async-task-queue)
                  (lambda ()
                    (set-future-values! future (values->list (apply proc args)))))
    future))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...) '()))))

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

