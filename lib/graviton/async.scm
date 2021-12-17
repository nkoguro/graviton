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
  (use graviton.misc)
  (use srfi-1)
  (use srfi-42)
  (use util.match)

  (export current-worker
          current-priority
          worker-event-wait-timeout
          worker-process-event-start-hook
          worker-start-hook
          <worker>
          make-worker
          worker-run
          worker-close
          worker-active?
          worker-shutdown
          worker-wait
          all-workers
          worker-submit-task
          worker-yield!
          run-concurrent
          concurrent
          worker-dispatch-event
          <worker-callback>
          <procedure-callback>
          <event-callback>
          worker-callback
          worker-callback?
          invoke-worker-callback
          shift-callback
          scheduler-add!
          scheduler-delete!
          grv-sleep!
          main-worker
          grv-worker
          worker-event-loop-hook

          add-message-handler!
          delete-message-handler!
          define-message

          <channel>
          make-channel
          channel-send
          channel-close
          channel-closed?
          channel-recv

          <grv-promise>
          make-grv-promise
          grv-promise-set-thunk!
          grv-promise-set-values!
          grv-promise-get
          disable-async-wait
          ))

(select-module graviton.async)

;;;

(define current-worker (make-parameter #f))
(define current-priority (make-parameter #f))
(define worker-event-wait-timeout (make-parameter #f))
(define-window-context-slot workers '())

(define-class <worker> ()
  ((task-queue :init-form (make-task-queue))
   (event-table :init-form (make-hash-table 'eq?))
   (event-loop-hook :init-form (make-hook))
   (name :init-keyword :name
         :init-value #f)
   (threads :init-value '())))

(define-method write-object ((obj <worker>) port)
  (format port "#<worker ~s>" (~ obj'name)))

(define worker-process-event-start-hook (make-hook))

(define (worker-process-event timeout)
  (define (invoke-thunk priority thunk)
    (parameterize ((current-priority priority))
      (reset
        (thunk))))

  (let1 worker (current-worker)
    (run-hook worker-process-event-start-hook)

    (log-framework-debug "Start worker-process-event: ~s" worker)
    (run-hook (~ worker'event-loop-hook))
    (begin0
        (match (dequeue-task! worker timeout)
          ((? eof-object? eof)
           #f)
          (#f
           #t)
          (((? real? timestamp) (? symbol? event-name) result-callback args ...)
           (and-let* ((proc+priority (hash-table-get (~ worker'event-table) event-name #f))
                      (proc (car proc+priority))
                      (priority (cdr proc+priority)))
             (log-framework-debug "Process event: ~a in worker: ~s" event-name worker)
             (if (eq? priority 'high)
               (invoke-thunk priority (cut apply proc args))
               (enqueue-task! worker
                              (task-priority->value priority)
                              (cons priority (^() (apply (or result-callback values) (values->list (apply proc args)))))
                              :timestamp timestamp)))
           #t)
          (((? symbol? priority) . (? procedure? thunk))
           (log-framework-debug "Invoke thunk: ~s in worker: ~s" thunk worker)
           (invoke-thunk priority thunk)
           #t)
          (v
           (errorf "Invalid value in task-queue: ~s" v)))
      (log-framework-debug "End worker-process-event: ~s" worker))))

(define worker-start-hook (make-hook))

(define (worker-run-event-loop worker thunk)
  (guard (e (else (report-error e)
                  (app-exit 70)))
    (current-worker worker)
    (current-priority 'default)
    (run-hook worker-start-hook)

    (reset
      (thunk))
    (while (worker-process-event (worker-event-wait-timeout))
      #t)))

(define (make-worker thunk :key (name #f) (size 1))
  (rlet1 worker (make <worker> :name (format "~a:~a" (window-context-id) name))
    (window-context-slot-atomic-update! 'workers
      (lambda (lst)
        (cons worker lst)))
    (dotimes (i size)
      (let1 thread (make-thread (cut worker-run-event-loop worker thunk) (format "~s:~d" worker i))
        (push! (~ worker'threads) thread)))))

(define (worker-run worker)
  (dolist (thread (~ worker'threads))
    (when (eq? (thread-state thread) 'new)
      (thread-start! thread))))

(define (worker-close worker)
  (task-queue-close (~ worker'task-queue)))

(define (worker-active? worker)
  (not (task-queue-closed? (~ worker'task-queue))))

(define (worker-shutdown worker)
  (worker-close worker)
  (remove-all-tasks! worker)
  (window-context-slot-atomic-update! 'workers
    (lambda (workers)
      (delete worker workers)))
  (undefined))

(define (worker-wait worker :key (timeout #f))
  (let1 timeout-val (gensym)
    (for-each (lambda (thread)
                (when (eq? (thread-join! thread timeout timeout-val) timeout-val)
                  (thread-terminate! thread)
                  (log-error "~s is killed due to timeout" thread)))
              (~ worker'threads))
    (set! (~ worker'threads) '()))
  (undefined))

(define (all-workers)
  (window-context-slot-ref 'workers))

(define (worker-event-loop-hook :optional (worker (current-worker)))
  (~ worker'event-loop-hook))

(define-class <task-queue> ()
  ((mutex :init-form (make-mutex 'task-queue))
   (condition-variable :init-form (make-condition-variable))
   (queue-table :init-form (make-hash-table 'eqv?))
   (state-atom :init-form (atom 'active))))

(define-class <chronological-queue> ()
  ((queue :init-form (make-queue))
   (need-to-sort? :init-value #f)))

(define (make-chronological-queue)
  (make <chronological-queue>))

(define (chronological-queue-enqueue! cqueue t obj)
  (with-slots (queue need-to-sort?) cqueue
    (let1 last-time (car (queue-rear queue '(#f . #f)))
      (enqueue! queue (cons t obj))
      (unless (and last-time (<= last-time t))
        (set! need-to-sort? #t)))))

(define (chronological-queue-dequeue! cqueue)
  (with-slots (queue need-to-sort?) cqueue
    (when need-to-sort?
      (set! queue (list->queue (sort (dequeue-all! queue) (^(e1 e2) (< (car e1) (car e2))))))
      (set! need-to-sort? #f))
    (cdr (dequeue! queue '(#f . #f)))))

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

(define-constant EVENT-QUEUE-PRIORITY-VALUE 0)  ;; this value shoud be equal to high priority.

;; task := (priority . thunk) | (timestamp event args ...)
(define (enqueue-task! worker pvalue task :key (timestamp #f))
  (with-slots (task-queue) worker
    (when (task-queue-active? task-queue)
      (with-slots (mutex condition-variable queue-table) task-queue
        (with-locking-mutex mutex
          (lambda ()
            (let1 queue (or (hash-table-get queue-table pvalue #f)
                            (rlet1 queue (make-chronological-queue)
                              (hash-table-put! queue-table pvalue queue)))
              (chronological-queue-enqueue! queue (or timestamp (now-seconds)) task)
              (condition-variable-broadcast! condition-variable))))))))

(define (dequeue-task! worker :optional (timeout #f))
  (with-slots (task-queue) worker
    (cond
      ((task-queue-active? task-queue)
       (with-slots (mutex condition-variable queue-table) task-queue
         (mutex-lock! mutex)
         (or (and-let1 task (any (lambda (pvalue)
                                   (chronological-queue-dequeue! (hash-table-get queue-table pvalue)))
                                 (sort (hash-table-keys queue-table)))
               (mutex-unlock! mutex)
               task)
             (cond
               ((not timeout)
                (mutex-unlock! mutex condition-variable #f)
                (dequeue-task! worker #f))
               ((<= timeout 0)
                (mutex-unlock! mutex)
                #f)
               (else
                (let1 start-sec (now-seconds)
                  (if (mutex-unlock! mutex condition-variable timeout)
                    (dequeue-task! worker (- timeout (- (now-seconds) start-sec)))
                    #f)))))))
      (else
       (eof-object)))))

(define (remove-all-tasks! worker)
  (with-slots (task-queue) worker
    (with-slots (mutex queue-table) task-queue
      (with-locking-mutex mutex
        (lambda ()
          (hash-table-clear! queue-table))))))

(define (worker-submit-task worker thunk :key (priority 'default))
  (enqueue-task! worker (task-priority->value priority) (cons priority thunk))
  (log-framework-debug "Enqueued thunk: ~s for worker: ~s with priority: ~a" thunk worker priority))

(define (worker-yield!)
  ;; Get the current worker and current priority before shift. shift exits (parameterize ...), so
  ;; current-worker and current-priority will not work in shift.
  (let ((worker (current-worker))
        (priority (current-priority)))
    (shift cont
      (worker-submit-task worker cont :priority priority))))

(define (run-concurrent thunk)
  (worker-submit-task (current-worker) thunk :priority (current-priority)))

(define-syntax concurrent
  (syntax-rules ()
    ((_ body ...)
     (run-concurrent (lambda () body ...)))))

(define-method worker-fire-event ((worker <worker>) event :rest args)
  (enqueue-task! worker EVENT-QUEUE-PRIORITY-VALUE (list* (now-seconds) event #f args)))

(define (add-message-handler! message proc :key (priority 'default))
  (hash-table-put! (~ (current-worker)'event-table) message (cons proc priority)))

(define (delete-message-handler! message)
  (hash-table-delete! (~ (current-worker)'event-table) message))

(define-syntax define-message
  (syntax-rules (:priority)
    ((_ message (arg ...) :priority priority body ...)
     (add-message-handler! 'message (lambda (arg ...) body ...) :priority priority))
    ((_ message (arg ...) body ...)
     (add-message-handler! 'message (lambda (arg ...) body ...)))
    ((_ message args :priority priority body ...)
     (add-message-handler! 'message (lambda args body ...) :priority priority))
    ((_ messag args body ...)
     (add-message-handler! 'message (lambda args body ...)))))

(define-class <worker-callback> ()
  ((worker :init-keyword :worker)))

(define-class <procedure-callback> (<worker-callback>)
  ((procedure :init-keyword :procedure)
   (priority :init-keyword :priority)))

(define-method write-object ((obj <procedure-callback>) port)
  (format port "#<procedure-callback: ~s (worker: ~s, priority: ~a)>" (~ obj'procedure) (~ obj'worker) (~ obj'priority)))

(define-class <event-callback> (<worker-callback>)
  ((event-name :init-keyword :event-name)
   (arg-transformer :init-keyword :arg-transformer)))

(define-method write-object ((obj <event-callback>) port)
  (format port "#<event-callback: ~a (worker: ~s)>" (~ obj'event-name) (~ obj'worker)))

(define-method worker-callback ((proc <procedure>) :key (priority #f) (worker #f))
  (make <procedure-callback> :worker (or worker (current-worker)) :procedure proc :priority (or priority (current-priority))))

(define-method worker-callback ((event-name <symbol>) :optional (arg-transformer #f))
  (make <event-callback> :worker (current-worker) :event-name event-name :arg-transformer (or arg-transformer values)))

(define (worker-callback? obj)
  (is-a? obj <worker-callback>))

(define-method invoke-worker-callback ((callback <procedure-callback>) arg-creator)
  (with-slots (worker procedure priority) callback
    (worker-submit-task worker (lambda ()
                                 (apply procedure (arg-creator)))
                        :priority priority)))

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

(define-syntax worker-shift
  (syntax-rules ()
    ((_ cont expr ...)
     (let ((worker (current-worker))
           (priority (current-priority)))
       (shift cont
         (parameterize ((current-worker worker)
                        (current-priority priority))
           expr ...))))))

(define-syntax shift-callback
  (syntax-rules ()
    ((_ callback expr ...)
     (worker-shift cont
       (let1 callback (worker-callback cont)
         expr ...)))))

(define-method worker-call-event ((worker <worker>) event :rest args)
  (shift-callback callback
    (enqueue-task! worker EVENT-QUEUE-PRIORITY-VALUE (list* (now-seconds) event callback args))))

(define-method worker-dispatch-event ((worker <worker>) event :rest args)
  (let1 gpromise (make-grv-promise)
    (enqueue-task! worker EVENT-QUEUE-PRIORITY-VALUE (list*
                                                       (now-seconds)
                                                       event
                                                       (lambda vals (grv-promise-set-values! gpromise vals))
                                                       args))
    gpromise))

(define-method object-apply ((worker <worker>) event :rest args)
  (apply worker-dispatch-event worker event args))

;;;

(define-class <worker-wrapper> ()
  ((wparam :init-keyword :wparam)))

(define (unwrap-worker worker-wrapper)
  ((~ worker-wrapper'wparam)))

(define-syntax grv-worker
  (er-macro-transformer
    (lambda (form rename id=?)
      (let loop ((args (cdr form))
                 (specs '()))
        (match args
          (((? keyword? kw) val rest ...)
           (loop rest (append specs (list kw val))))
          ((body ...)
           (quasirename rename `(let1 provider (lambda ()
                                                 (rlet1 worker (apply make-worker (lambda () ,@body) (list ,@specs))
                                                   (worker-run worker)))
                                  (if (window-context)
                                    (provider)
                                    (make <worker-wrapper> :wparam (make-window-parameter* provider)))))))))))

(define-method worker-fire-event ((worker-wrapper <worker-wrapper>) event :rest args)
  (apply worker-fire-event (unwrap-worker worker-wrapper) event args))

(define-method worker-call-event ((worker-wrapper <worker-wrapper>) event :rest args)
  (apply worker-call-event (unwrap-worker worker-wrapper) event args))

(define-method worker-dispatch-event ((worker-wrapper <worker-wrapper>) event :rest args)
  (apply worker-dispatch-event (unwrap-worker worker-wrapper) event args))

(define-method object-apply ((worker-wrapper <worker-wrapper>) event :rest args)
  (apply worker-dispatch-event (unwrap-worker worker-wrapper) event args))

;;;

(define scheduler
  (grv-worker
   :name "scheduler"
   ;; schedule := (time-in-sec callback interval-in-sec)
   (let1 schedule-list '()
     (define (update-event-wait-timeout!)
       (worker-event-wait-timeout (if (null? schedule-list)
                                     #f
                                     (max 0 (- (first (car schedule-list)) (now-seconds))))))

     (define (add-schedule! time-in-sec callback interval-in-sec)
       (set! schedule-list (sort (cons (list time-in-sec callback interval-in-sec)
                                       schedule-list)
                                 (lambda (s1 s2)
                                   (< (first s1) (first s2)))))
       (update-event-wait-timeout!))

     (define (del-schedule! callback)
       (set! schedule-list (remove (match-lambda
                                     ((_ cb _ ...)
                                      (equal? cb callback))
                                     (schedule
                                      (errorf "Invalid schedule: ~s" schedule)))
                                   schedule-list))
       (update-event-wait-timeout!))

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
       (update-event-wait-timeout!))

     (add-hook! (worker-event-loop-hook) process-schedule!)
     (add-message-handler! 'add add-schedule!)
     (add-message-handler! 'del del-schedule!))))

;; thunk-or-event := procedure | (event-name args ...)
(define (scheduler-add! callback :key ((:at abs-time) #f) ((:after rel-sec) #f) ((:every interval) #f))
  (unless (or (and abs-time (not rel-sec) (not interval))
              (and (not abs-time) rel-sec (not interval))
              (and (not abs-time) (not rel-sec) interval))
    (error "scheduler-add! needs :at, :after or :every keyword"))
  (worker-fire-event scheduler
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
  (worker-fire-event scheduler 'del callback))

(define (grv-sleep! time-or-sec)
  (shift-callback callback
    (apply scheduler-add! callback
           (cond
             ((time? time-or-sec)
              (list :at time-or-sec))
             ((real? time-or-sec)
              (list :after time-or-sec))
             (else
              (errorf "<time> or <real> required, but got ~s" time-or-sec))))))

;;;

(define-window-context-slot main-worker #f)

(define (main-worker)
  (window-context-slot-ref 'main-worker))

;;;

(define-class <channel> ()
  ((lock :init-form (make-mutex))
   (queue :init-form (make-queue))
   (closed? :init-value #f)
   (callbacks :init-value '())))

(define (make-channel)
  (make <channel>))

(define (%notify-update! channel)
  (with-slots (callbacks) channel
    (for-each (^c (c)) callbacks)
    (set! callbacks '())))

(define (channel-send channel :rest vals)
  (with-slots (lock queue closed?) channel
    (with-locking-mutex lock
      (lambda ()
        (when closed?
          (errorf "I/O attempted on closed channel: ~s" channel))
        (for-each (^v (enqueue! queue v)) vals)
        (%notify-update! channel)))))

(define (channel-close channel)
  (with-slots (lock closed?) channel
    (with-locking-mutex lock
      (lambda ()
        (set! closed? #t)
        (%notify-update! channel)))))

(define (channel-closed? channel)
  (with-slots (lock closed? callbacks) channel
    (with-locking-mutex lock
      (lambda ()
        closed?))))

(define (channel-recv channel :optional fallback)
  (with-slots (lock queue closed? callbacks) channel
    (mutex-lock! lock)
    (cond
      ((not (queue-empty? queue))
       (begin0
           (dequeue! queue)
         (mutex-unlock! lock)))
      (closed?
       (begin0
           (eof-object)
         (mutex-unlock! lock)))
      ((undefined? fallback)
       (shift-callback callback
         (push! callbacks callback)
         (mutex-unlock! lock))
       (channel-recv channel))
      (else
       fallback))))

(define-method object-apply ((channel <channel>))
  (channel-recv channel))

;;;

(define-class <grv-promise> ()
  ((lock :init-form (make-mutex))
   (condition-variable :init-form (make-condition-variable))
   (value-list :init-value #f)
   (arg-creator :init-value #f)
   (callbacks :init-value '())))

(define (make-grv-promise)
  (make <grv-promise>))

(define (grv-promise-set-thunk! gpromise thunk)
  (with-slots (lock condition-variable arg-creator value-list callbacks) gpromise
    (with-locking-mutex lock
      (lambda ()
        (when value-list
          (errorf "<grv-promise> already has values: ~s" gpromise))
        (when arg-creator
          (errorf "<grv-promise> already has arg-creator: ~s" gpromise))
        (set! arg-creator thunk)
        (for-each (^c (c)) callbacks)
        (set! callbacks '())))
    (condition-variable-broadcast! condition-variable)))

(define (grv-promise-set-values! gpromise vals)
  (with-slots (lock condition-variable arg-creator value-list callbacks) gpromise
    (with-locking-mutex lock
      (lambda ()
        (when value-list
          (errorf "<grv-promise> already has values: ~s" gpromise))
        (when arg-creator
          (errorf "<grv-promise> already has arg-creator: ~s" gpromise))
        (set! value-list vals)
        (for-each (^c (c)) callbacks)
        (set! callbacks '())))
    (condition-variable-broadcast! condition-variable)))

(define disable-async-wait (make-parameter #f))

(define (grv-promise-get gpromise :rest fallback-values)
  (with-slots (lock condition-variable value-list arg-creator callbacks) gpromise
    (mutex-lock! lock)
    (cond
      (value-list
       (begin0
           (apply values value-list)
         (mutex-unlock! lock)))
      (arg-creator
       (set! value-list (arg-creator))
       (set! arg-creator #f)
       (begin0
           (apply values value-list)
         (mutex-unlock! lock)))
      ((null? fallback-values)
       (cond
         ((disable-async-wait)
          (mutex-unlock! lock condition-variable))
         (else
          (shift-callback callback
            (push! callbacks callback)
            (mutex-unlock! lock))))
       (grv-promise-get gpromise))
      (else
       (begin0
           (apply values fallback-values)
         (mutex-unlock! lock))))))

(define-method object-apply ((grv-promise <grv-promise>))
  (grv-promise-get grv-promise))
