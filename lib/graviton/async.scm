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
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.threads)
  (use graviton.app)
  (use graviton.misc)
  (use srfi-1)
  (use srfi-42)
  (use util.match)

  (export current-worker
          worker-thread-idle-timeout
          <worker>
          <worker-thread>
          make-worker
          make-worker-thread
          worker-begin
          run-worker-thread
          worker-thread-begin
          worker-run
          worker-close
          worker-shutdown
          worker-thread-wait
          all-worker-threads
          worker-submit-task
          worker-yield!
          worker-fire-event
          on-event
          on-idle
          <worker-callback>
          <procedure-callback>
          <event-callback>
          worker-callback
          worker-callback?
          scheduler-add!
          scheduler-delete!
          worker-sleep!
          main-worker-thunk-set!
          main-worker
          ))

(select-module graviton.async)

;;;

(define current-worker (make-parameter #f))
(define current-priority (make-parameter #f))
(define worker-thread-idle-timeout (make-parameter #f))
(define-application-context-slot worker-threads '())

(define-class <worker> ()
  ((task-queue :init-form (make-task-queue))
   (event-table :init-form (make-hash-table 'eq?))
   (idle-handler :init-value #f)))

(define-class <worker-thread> (<worker>)
  ((name :init-keyword :name
         :init-value #f)
   (threads :init-value '())))

(define-method write-object ((obj <worker-thread>) port)
  (format port "#<worker-thread ~s>" (~ obj'name)))

(define *dequeue-timeout-val* (gensym))

(define (worker-process-event worker :optional (timeout #f))
  (parameterize ((current-worker worker))
    (match (dequeue-task+priority! worker timeout *dequeue-timeout-val*)
      ((? eof-object? eof)
       #f)
      ((? (cut eq? <> *dequeue-timeout-val*) _)
       (and-let1 thunk (~ worker'idle-handler)
         (parameterize ((current-priority 'low))
           (reset
             (thunk))))
       #t)
      (((? procedure? task) . (? symbol? priority))
       (parameterize ((current-priority priority))
         (reset
           (task)))
       #t)
      ((((? symbol? event-name) args ...) . (? symbol? priority))
       (let* ((proc+priority (or (hash-table-get (~ worker'event-table) event-name #f)
                                 (errorf "Event: ~a not found: ~s" event-name args)))
              (proc (car proc+priority))
              (event-priority (cdr proc+priority)))
         (cond
           ((eq? priority event-priority)
            (parameterize ((current-priority priority))
              (reset
                (apply proc args))))
           (else
            (worker-submit-task worker (^() (apply proc args)) :priority event-priority))))
       #t)
      (else
       (errorf "Invalid task: ~s" task+priority)))))

(define (make-worker thunk)
  (rlet1 worker (make <worker>)
    (enqueue-task! worker thunk)))

(define-method worker-run ((worker <worker>))
  (worker-process-event worker 0))

(define-method object-apply ((worker <worker>))
  (worker-run worker))

(define-syntax worker-begin
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ body ...)
         (let1 worker-box (box #f)
           (quasirename rename
             `(if-let1 worker (unbox ,worker-box)
                (worker-run worker)
                (let1 worker (lambda () body ...)
                   (set-box! ,worker-box worker)
                   (worker-run worker))))))))))

(define (worker-thread-run-event-loop worker-thread thunk)
  (guard (e (else (report-error e)
                  (app-exit 70)))
    (parameterize ((current-worker worker-thread)
                   (current-priority 'default))
      (reset
        (thunk)))
    (while (worker-process-event worker-thread (worker-thread-idle-timeout))
      #t)))

(define (make-worker-thread thunk :key (name #f) (size 1))
  (rlet1 worker-thread (make <worker-thread> :name name)
    (application-context-slot-atomic-update! 'worker-threads
      (lambda (lst)
        (cons worker-thread lst)))
    (dotimes (size)
      (let1 thread (make-thread (cut worker-thread-run-event-loop worker-thread thunk) (format "~s" worker-thread))
        (push! (~ worker-thread'threads) thread)))))

(define-method worker-run ((worker-thread <worker-thread>))
  (dolist (thread (~ worker-thread'threads))
    (when (eq? (thread-state thread) 'new)
      (thread-start! thread))))

(define (run-worker-thread thunk :key (name #f) (size 1))
  (rlet1 worker-thread (make-worker-thread thunk :name name :size size)
    (worker-run worker-thread)))

(define-syntax worker-thread-begin
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ name (specs ...) body ...)
         (quasirename rename `(rlet1 worker-thread (apply make-worker-thread (lambda () body ...) :name name specs)
                                (worker-run worker-thread))))
        (_
         (errorf "malformed worker-thread: ~s" form))))))

(define (worker-close worker)
  (task-queue-close (~ worker'task-queue)))

(define (worker-shutdown worker)
  (worker-close worker)
  (remove-all-tasks! worker)
  (undefined))

(define (worker-thread-wait worker-thread :key (timeout #f))
  (let1 timeout-val (gensym)
    (for-each (lambda (thread)
                (when (eq? (thread-join! thread timeout timeout-val) timeout-val)
                  (thread-terminate! thread)
                  (log-error "~s is killed due to timeout" thread)))
              (~ worker-thread'threads))
    (set! (~ worker-thread'threads) '()))
  (undefined))

(define (all-worker-threads)
  (application-context-slot-ref 'worker-threads))

(define-class <task-queue> ()
  ((mutex :init-form (make-mutex 'task-queue))
   (condition-variable :init-form (make-condition-variable))
   (queue-table :init-form (make-hash-table 'eqv?))
   (state-atom :init-form (atom 'active))))

(define (make-task-queue)
  (make <task-queue>))

(define (task-queue-close tq)
  (atomic-update! (~ tq'state-atom)
    (lambda (_)
      'closed))
  (with-slots (mutex condition-variable) tq
    (with-locking-mutex mutex
      (lambda ()
        (condition-variable-broadcast! condition-variable)))))

(define (task-queue-active? tq)
  (eq? (atom-ref (~ tq'state-atom)) 'active))

(define (task-queue-closed? tq)
  (not (task-queue-active? tq)))

(define (task-priority->value priority)
  (case priority
    ((high) 0)
    ((default) 10)
    ((low) 100)
    (else
     (errorf "Invalid task priority: ~s" priority))))

(define (enqueue-task! worker task :key (priority 'default))
  (with-slots (task-queue) worker
    (when (task-queue-active? task-queue)
      (with-slots (mutex condition-variable queue-table) task-queue
        (with-locking-mutex mutex
          (lambda ()
            (let* ((pvalue (task-priority->value priority))
                   (queue (or (hash-table-get queue-table pvalue #f)
                              (rlet1 queue (make-queue)
                                (hash-table-put! queue-table pvalue queue)))))
              (enqueue! queue (cons task priority))
              (condition-variable-broadcast! condition-variable))))))))

(define (dequeue-task+priority! worker :optional (timeout #f) (timeout-val #f))
  (with-slots (task-queue) worker
    (cond
      ((task-queue-active? task-queue)
       (with-slots (mutex condition-variable queue-table) task-queue
         (mutex-lock! mutex)
         (let1 task+priority (any (lambda (pvalue)
                                    (dequeue! (hash-table-get queue-table pvalue) #f))
                                  (sort (hash-table-keys queue-table)))
           (cond
             (task+priority
              (mutex-unlock! mutex)
              task+priority)
             (else
              (cond
                ((equal? timeout 0)
                 (mutex-unlock! mutex)
                 timeout-val)
                (else
                 (if (mutex-unlock! mutex condition-variable timeout)
                   (dequeue-task+priority! worker)
                   timeout-val))))))))
      (else
       (eof-object)))))

(define (remove-all-tasks! worker)
  (with-slots (task-queue) worker
    (with-slots (mutex queue-table) task-queue
      (with-locking-mutex mutex
        (lambda ()
          (hash-table-clear! queue-table))))))

(define (worker-submit-task worker thunk :key (priority 'default))
  (enqueue-task! worker thunk :priority priority))

(define (worker-yield!)
  (shift cont
    (worker-submit-task (current-worker) cont :priority (current-priority))))

(define (register-event-handler! event proc :key (priority 'default))
  (hash-table-put! (~ (current-worker)'event-table) event (cons proc priority)))

(define (unregister-event-handler! event)
  (hash-table-delete! (~ (current-worker)'event-table) event))

(define (worker-fire-event worker event :rest args)
  ;; The event will be re-priorized in the event loop.
  (enqueue-task! worker (cons event args) :priority 'high))

(define-syntax on-event
  (syntax-rules (:priority)
    ((_ (event :priority priority) (arg ...) body ...)
     (register-event-handler! event (lambda (arg ...) body ...) :priority priority))
    ((_ (event :priority priority) proc)
     (register-event-handler! event proc :priority priority))
    ((_ event (arg ...) body ...)
     (register-event-handler! event (lambda (arg ...) body ...)))
    ((_ event proc)
     (register-event-handler! event proc))))

(define (register-idle-handler! proc)
  (set! (~ (current-worker)'idle-handler) proc))

(define-syntax on-idle
  (syntax-rules ()
    ((_ (arg ...) body ...)
     (register-idle-handler! (lambda (arg ...) body ...)))
    ((_ proc)
     (register-idle-handler! proc))))

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
      (worker-thread-idle-timeout (if (null? schedule-list)
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

    (on-idle process-schedule!)
    (on-event 'add add-schedule!)
    (on-event 'del del-schedule!)))

(define (run-scheduler)
  (run-worker-thread scheduler :name "scheduler"))

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

(define *main-worker-thunk* #f)

(define (main-worker-thunk-set! thunk)
  (set! *main-worker-thunk* thunk))

(define-application-context-slot main-worker (rlet1 wt (make-worker-thread (or *main-worker-thunk*
                                                                               (lambda () (grv-exit 70)))
                                                                           :name "main")
                                               (worker-run wt)))

(define (main-worker)
  (application-context-slot-ref 'main-worker))
