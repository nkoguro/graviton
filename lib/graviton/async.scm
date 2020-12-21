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
  (use graviton.misc)
  (use graviton.scheduler)
  (use srfi-1)
  (use srfi-42)
  (use util.match)

  (export current-worker
          worker-idle-timeout
          <worker>
          run-worker
          worker-shutdown!
          worker-wait
          all-workers
          worker-submit-task
          worker-yield!
          worker-fire-event
          on-event
          <worker-callback>
          <procedure-callback>
          <event-callback>
          worker-callback
          worker-callback?
          scheduler-add!
          scheduler-delete!
          worker-sleep!
          main-worker
          ))

(select-module graviton.async)

;;;

(define current-worker (make-parameter #f))
(define current-priority (make-parameter #f))
(define worker-idle-timeout (make-parameter #f))
(define-application-context-slot workers '())

(define-class <worker> ()
  ((name :init-keyword :name)
   (task-queue :init-form (make-task-queue))
   (event-table-atom :init-form (atom (make-hash-table 'eq?)))
   (threads-atom :init-form (atom '()))))

(define-method write-object ((obj <worker>) port)
  (format port "#<worker ~s>" (~ obj'name)))

(define (run-event-loop worker thunk)
  (guard (e (else (report-error e)
                  (app-exit 70)))
    (worker-register-thread! worker (current-thread))
    (current-worker worker)
    (let1 timeout-val (gensym)
      (define (next-task+priority)
        (dequeue-task+priority! worker (worker-idle-timeout) timeout-val))
      (unwind-protect
          (let loop ((task+priority (cons thunk 'default)))
            (match task+priority
              (((? eof-object? eof) . _)
               #t)
              ((? (cut eq? <> timeout-val) _)
               (worker-fire-event worker 'idle)
               (loop (next-task+priority)))
              (((? procedure? task) . (? symbol? priority))
               (current-priority priority)
               (reset
                 (task))
               (loop (next-task+priority)))
              ((((? symbol? event-name) args ...) . (? symbol? priority))
               (let* ((proc+priority (or (atomic (~ worker'event-table-atom) (cut hash-table-get <> event-name #f))
                                         (errorf "Event: ~a not found: ~s" event-name args)))
                      (proc (car proc+priority))
                      (event-priority (cdr proc+priority)))
                 (cond
                   ((eq? priority event-priority)
                    (current-priority priority)
                    (reset
                      (apply proc args)))
                   (else
                    (worker-submit-task worker (^() (apply proc args)) :priority event-priority))))
               (loop (next-task+priority)))
              (else
               (errorf "Invalid task: ~s" task+priority))))
        (worker-unregister-thread! worker (current-thread))))))

(define (run-worker thunk :key (name #f) (size 1))
  (unless (application-context)
    (error "worker-start! is called without application-context"))
  (rlet1 worker (make <worker> :name name)
    (application-context-slot-atomic-update! 'workers
      (lambda (lst)
        (cons worker lst)))
    (dotimes (size)
      (let1 thread (make-thread (cut run-event-loop worker thunk) (format "~s" worker))
        (thread-start! thread)))))

(define (worker-register-thread! worker thread)
  (atomic-update! (~ worker'threads-atom)
    (lambda (threads)
      (cons thread threads))))

(define (worker-unregister-thread! worker thread)
  (atomic-update! (~ worker'threads-atom)
    (lambda (threads)
      (rlet1 lst (delete thread threads)
        (when (= (length lst) 0)
          (application-context-slot-atomic-update! 'workers (cut delete worker <>)))))))

(define (worker-shutdown! worker :key (priority 'high))
  (dotimes ((atomic (~ worker'threads-atom) length))
    (enqueue-task! worker (eof-object) :priority priority))
  (undefined))

(define (worker-wait worker :key (timeout #f))
  (let1 timeout-val (gensym)
    (for-each (lambda (thread)
                (when (eq? (thread-join! thread timeout timeout-val) timeout-val)
                  (thread-terminate! thread)
                  (log-error "~s is killed due to timeout" thread)))
              (atom-ref (~ worker'threads-atom))))
  (undefined))

(define (all-workers)
  (application-context-slot-ref 'workers))

(define-class <task-queue> ()
  ((mutex :init-form (make-mutex 'task-queue))
   (condition-variable :init-form (make-condition-variable))
   (queue-table :init-form (make-hash-table 'eqv?))))

(define (make-task-queue)
  (make <task-queue>))

(define (task-priority->value priority)
  (case priority
    ((high) 0)
    ((default) 10)
    ((low) 100)
    (else
     (errorf "Invalid task priority: ~s" priority))))

(define (enqueue-task! worker task :key (priority 'default))
  (with-slots (mutex condition-variable queue-table) (~ worker'task-queue)
    (with-locking-mutex mutex
      (lambda ()
        (let* ((pvalue (task-priority->value priority))
               (queue (or (hash-table-get queue-table pvalue #f)
                          (rlet1 queue (make-queue)
                            (hash-table-put! queue-table pvalue queue)))))
          (enqueue! queue (cons task priority))
          (condition-variable-broadcast! condition-variable))))))

(define (dequeue-task+priority! worker :optional (timeout #f) (timeout-val #f))
  (with-slots (mutex condition-variable queue-table) (~ worker'task-queue)
    (mutex-lock! mutex)
    (let1 task+priority (any (lambda (pvalue)
                               (dequeue! (hash-table-get queue-table pvalue) #f))
                             (sort (hash-table-keys queue-table)))
      (cond
        (task+priority
         (mutex-unlock! mutex)
         task+priority)
        (else
         (if (mutex-unlock! mutex condition-variable timeout)
           (dequeue-task+priority! worker)
           timeout-val))))))

(define (worker-submit-task worker thunk :key (priority 'default))
  (enqueue-task! worker thunk :priority priority))

(define (worker-yield!)
  (shift cont
    (worker-submit-task (current-worker) cont :priority (current-priority))))

(define (worker-register-event! worker event proc :key (priority 'default))
  (atomic (~ worker'event-table-atom)
    (lambda (tbl)
      (hash-table-put! tbl event (cons proc priority)))))

(define (worker-unregister-event! worker event)
  (atomic (~ worker'event-table-atom)
    (lambda (tbl)
      (hash-table-delete! tbl event))))

(define (worker-fire-event worker event :rest args)
  ;; The event will be re-priorized in the event loop.
  (enqueue-task! worker (cons event args) :priority 'high))

(define-syntax on-event
  (syntax-rules (:priority)
    ((_ (event :priority priority) (arg ...) body ...)
     (worker-register-event! (current-worker) event (lambda (arg ...) body ...) :priority priority))
    ((_ (event :priority priority) proc)
     (worker-register-event! (current-worker) event proc :priority priority))
    ((_ event (arg ...) body ...)
     (worker-register-event! (current-worker) event (lambda (arg ...) body ...)))
    ((_ event proc)
     (worker-register-event! (current-worker) event proc))))

(define-class <worker-callback> ()
  ((worker :init-keyword :worker)))

(define-class <procedure-callback> (<worker-callback>)
  ((procedure :init-keyword :procedure)
   (priority :init-keyword :priority)))

(define-class <event-callback> (<worker-callback>)
  ((event-name :init-keyword :event-name)
   (arg-transformer :init-keyword :arg-transformer)))

(define-method worker-callback ((proc <procedure>) :key (priority #f))
  (make <procedure-callback> :worker (current-worker) :procedure proc :priority (or priority (current-priority))))

(define-method worker-callback ((event-name <symbol>) :optional (arg-transformer #f))
  (make <event-callback> :worker (current-worker) :event-name event-name :arg-transformer (or arg-transformer values)))

(define (worker-callback? obj)
  (is-a? obj <worker-callback>))

(define-method object-apply ((callback <procedure-callback>) :rest args)
  (with-slots (worker procedure priority) callback
    (worker-submit-task worker (cut apply procedure args) :priority priority)))

(define-method object-apply ((callback <event-callback>) :rest args)
  (with-slots (worker event-name arg-transformer) callback
    (apply worker-fire-event worker event-name (arg-transformer args))))

(define-method object-equal? ((callback1 <procedure-callback>) (callback2 <procedure-callback>))
  (every (^x (equal? (~ callback1 x) (~ callback2 x))) '(worker procedure priority)))

(define-method object-equal? ((callback1 <event-callback>) (callback2 <event-callback>))
  (every (^x (equal? (~ callback1 x) (~ callback2 x))) '(worker event-name)))

;;;

(define (scheduler)
  ;; schedule := (time-in-sec callback interval-in-sec)
  (let1 schedule-list '()
    (define (update-idle-timeout!)
      (worker-idle-timeout (if (null? schedule-list)
                             #f
                             (max 0 (- (first (car schedule-list)) (now-seconds))))))

    (define (add-schedule! time-in-sec callback interval-in-sec)
      (set! schedule-list (sort (cons (list time-in-sec callback interval-in-sec)
                                      schedule-list)
                                (lambda (s1 s2)
                                  (< (first s1) (first s2)))))
      (update-idle-timeout!))

    (define (del-schedule! callback)
      (set! schedule-list (remove (match-lambda
                                    ((_ cb _ ...)
                                     (equal? cb callback))
                                    (schedule
                                     (errorf "Invalid schedule: ~s" schedule)))
                                  schedule-list))
      (update-idle-timeout!))

    (define (process-schedule!)
      (let loop ()
        (unless (null? schedule-list)
          (match-let1 (time-in-sec callback interval-in-sec) (car schedule-list)
            (let1 now (now-seconds)
              (when (<= time-in-sec now)
                (callback)
                (pop! schedule-list)
                (when interval-in-sec
                  (add-schedule! (+ now interval-in-sec) callback interval-in-sec))
                (loop))))))
      (update-idle-timeout!))

    (on-event 'idle process-schedule!)
    (on-event 'add add-schedule!)
    (on-event 'del del-schedule!)))

(define (run-scheduler)
  (run-worker scheduler :name "scheduler"))

(define-application-context-slot scheduler (run-scheduler))

;; thunk-or-event := procedure | (event-name args ...)
(define (scheduler-add! callback :key ((:at abs-time) #f) ((:after rel-sec) #f) ((:every interval) #f))
  (unless (or (and abs-time (not rel-sec) (not interval))
              (and (not abs-time) rel-sec (not interval))
              (and (not abs-time) (not rel-sec) interval))
    (error "scheduler-add! needs :at, :after or :every keyword"))
  (worker-fire-event (application-context-slot-ref 'scheduler)
                     'add
                     (cond
                             (abs-time
                              (time->seconds abs-time))
                             (rel-sec
                              (+ (now-seconds) rel-sec))
                             (interval
                              (+ (now-seconds) interval)))
                     callback
                     interval))

(define (scheduler-delete! callback)
  (worker-fire-event (application-context-slot-ref 'scheduler) 'del callback))

(define (worker-sleep! time-or-sec)
  (shift cont
    (apply scheduler-add! (worker-callback cont)
           (cond
             ((time? time-or-sec)
              (list :at time-or-sec))
             ((real? time-or-sec)
              (list :after time-or-sec))
             (else
              (errorf "<time> or <real> required, but got ~s" time-or-sec))))))

;;;

(define-application-context-slot main-worker (run-worker (lambda () #t) :name "main"))

(define (main-worker)
  (application-context-slot-ref 'main-worker))
