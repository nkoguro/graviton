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
  (use control.thread-pool)
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

          task-queue-worker-timeout
          task-queue-worker-timeout-set!
          task-queue-shutdown-timeout
          task-queue-shutdown-timeout-set!
          task-queue-shutdown!

          task-quit

          current-pool
          async-apply
          async

          submit-task))

(select-module graviton.async)

;;;

(define task-start-hook (make-hook))
(define task-end-hook (make-hook))

(define current-pool (make-parameter #f))

(define-application-context-slot task-queue #f)

(define-class <task-queue> ()
  ((lock :init-form (make-mutex))
   (state :init-value 'active)
   (worker-timeout :init-keyword :worker-timeout)
   (name :init-keyword :name)
   (workers :init-value '())))

(define-method write-object ((tq <task-queue>) port)
  (if-let1 name (slot-ref tq 'name)
           (format port "#<task-queue:~a>" name)
           (display "#<task-queue>" port)))

(define-class <task-worker> ()
  ((lock :init-form (make-mutex))
   (queue :init-form (make-mtqueue))
   (busy? :init-value #f)
   (thread :init-value #f)))

(define (task-worker-busy? tw)
  (with-locking-mutex (slot-ref tw 'lock)
    (lambda ()
      (slot-ref tw 'busy?))))

(define (task-worker-busy-set! tw busy?)
  (with-locking-mutex (slot-ref tw 'lock)
    (lambda ()
      (slot-set! tw 'busy? busy?))))

(define (task-worker-add-job! tw thunk)
  (enqueue! (slot-ref tw 'queue) thunk))

(define (task-worker tq tw)
  (task-worker-register! tq tw)

  (while (dequeue/wait! (slot-ref tw 'queue) (task-queue-safe-slot-ref tq 'worker-timeout) #f)
    => thunk
    (unwind-protect
        (begin
          (task-worker-busy-set! tw #t)
          (thunk))
      (task-worker-busy-set! tw #f)))

  (task-worker-unregister! tq tw)
  #t)

(define (start-task-worker tq)
  (with-task-queue-lock tq
    (lambda ()
      (let* ((worker-id (+ (length (slot-ref tq 'workers)) 1))
             (name (and-let1 prefix (slot-ref tq 'name)
                     (format #f "~a:~d-~d" prefix (application-context-id) worker-id)))
             (tw (make <task-worker>))
             (thread (make-thread (cut task-worker tq tw) name)))
        (slot-set! tw 'thread thread)
        (thread-start! thread)
        tw))))

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

(define (task-queue-worker-timeout tq)
  (task-queue-safe-slot-ref tq 'worker-timeout))

(define (task-queue-worker-timeout-set! tq worker-timeout)
  (with-task-queue-lock tq
    (lambda ()
      (slot-set! tq 'worker-timeout worker-timeout))))

(define (task-worker-register! tq worker)
  (with-task-queue-lock tq
    (lambda ()
      (push! (slot-ref tq 'workers) worker))))

(define (task-worker-unregister! tq worker)
  (with-task-queue-lock tq
      (lambda ()
        (slot-set! tq 'workers (remove (cut eq? worker <>) (slot-ref tq 'workers))))))

(define (make-task-queue worker-timeout name)
  (make <task-queue> :worker-timeout worker-timeout :name name))

(define task-quit-proc (make-parameter #f))

(define (task-quit)
  (let1 proc (task-quit-proc)
    (unless proc
      (error "task-quit can't be called outside task."))
    (proc)))

(define (submit-task pool thunk)
  (let* ((app-context (application-context))
         (wrapped-thunk (lambda ()
                          (call/cc (lambda (cont)
                                     (parameterize ((application-context app-context)
                                                    (current-pool pool)
                                                    (task-quit-proc cont))
                                       (guard (e (else (report-error e)
                                                       (app-exit 70)))
                                         (thunk)))
                                     (undefined))))))
    (cond
      (pool
       (add-job! pool wrapped-thunk))
      ((application-context-slot-ref 'task-queue)
       => (lambda (tq)
            (with-task-queue-lock tq
              (lambda ()
                (let1 worker (or (find (lambda (tw)
                                         (not (task-worker-busy? tw)))
                                       (slot-ref tq 'workers))
                                 (start-task-worker tq))
                  (task-worker-add-job! worker wrapped-thunk))))))
      (else
       (error "Neither thread-pool nor task-queue is specified.")))))

(define (task-queue-close! task-queue)
  (with-task-queue-lock task-queue
    (lambda ()
      (slot-set! task-queue 'state 'inactive)
      (for-each (lambda (tw)
                  (task-worker-add-job! tw #f))
                (slot-ref task-queue 'workers)))))

(define (task-queue-join! task-queue :optional (timeout #f))
  (with-task-queue-lock task-queue
    (lambda ()
      (when (eq? (slot-ref task-queue 'state) 'active)
        (errorf "task-queue ~s is still active" task-queue))))

  (fold (lambda (tw timeout)
          (let ((thread (slot-ref tw 'thread))
                (start (time->seconds (current-time))))
            (unless (guard (e (else (report-error e)))
                      (thread-join! thread timeout #f))
              (thread-terminate! thread))
            (if timeout
                (let1 end (time->seconds (current-time))
                  (max 0 (- timeout (- end start))))
                #f)))
        timeout
        (task-queue-safe-slot-ref task-queue 'workers)))

(define *task-queue-shutdown-timeout* 1)

(define (task-queue-shutdown-timeout-set! v)
  (set! *task-queue-shutdown-timeout* v))

(define (task-queue-shutdown-timeout)
  *task-queue-shutdown-timeout*)

(define (task-queue-shutdown!)
  (let1 tq (application-context-slot-ref 'task-queue)
    (task-queue-close! tq)
    (task-queue-join! tq (task-queue-shutdown-timeout))
    (slot-set! tq 'workers '())))

(add-hook! app-close-hook task-queue-shutdown!)

;;;

(define (init-task-queue)
  (application-context-slot-set! 'task-queue (make-task-queue #f "task-queue")))

(add-hook! app-start-hook init-task-queue)

;;;

(define (async-apply proc args)
  (let ((queue (make-mtqueue))
        (app-context (application-context)))
    (submit-task (current-pool)
      (lambda ()
        (enqueue! queue (values->list (apply proc args)))))
    (delay (dequeue/wait! queue))))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...) '()))))
