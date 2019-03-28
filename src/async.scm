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

(inline-stub
 (define-cfn notify-uncaught-exception (msg)
   ::void
   (let* ((event::SDL_Event))
     (set! (ref event type) graviton-event-type
           (ref event user code) GRAVITON_UNCAUGHT_EXCEPTION_CODE
           (ref event user data1) msg)
     (SDL_PushEvent (& event))))

 (define-cfn finalize-future (z data::void*)
   ::void
   (when (GRV_FUTURE_P z)
     (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR z)))
       (SDL_DestroyCond (-> gfuture cond))
       (SDL_DestroyMutex (-> gfuture lock))
       (when (and (not (-> gfuture consumed?)) (-> gfuture message))
         (notify-uncaught-exception (SCM_MAKE_STR_COPYING (-> gfuture message)))
         (free (-> gfuture message))))))

 (define-cfn make-future ()
   (let* ((gfuture::GrvFuture* (SCM_NEW GrvFuture))
          (obj (MAKE_GRV_FUTURE gfuture)))
     (set! (-> gfuture lock) (SDL_CreateMutex)
           (-> gfuture cond) (SDL_CreateCond)
           (-> gfuture result) SCM_FALSE
           (-> gfuture exception) SCM_FALSE
           (-> gfuture message) NULL
           (-> gfuture continuations) SCM_NIL
           (-> gfuture consumed?) false)
     (Scm_RegisterFinalizer obj finalize-future NULL)
     (return obj)))

 (define-cfn set-future-result! (gfuture::GrvFuture* result report-error?::bool)
   ::void
   (let* ((conts SCM_NIL)
          (err?::bool false))
     (SDL_LockMutex (-> gfuture lock))
     (cond
       ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
        (set! (-> gfuture result) result
              conts (-> gfuture continuations)
              (-> gfuture continuations) SCM_NIL)
        (SDL_CondSignal (-> gfuture cond)))
       (else
        (set! err? true)))
     (SDL_UnlockMutex (-> gfuture lock))
     (cond
       ((not err?)
        (unless (SCM_NULLP conts)
          (SDL_LockMutex (-> gfuture lock))
          (set! (-> gfuture consumed?) true)
          (SDL_UnlockMutex (-> gfuture lock))
          (for-each (lambda (cont)
                      (main-loop-apply cont (SCM_LIST2 result SCM_FALSE)))
                    conts)))
       (report-error?
        (Scm_Error "result has been already set in <graviton-future>")))))

 (.define MAX_MESSAGE_LENGTH 1024)
 ) ;; end of inline-stub

(define-cproc make-future ()
  (return (make-future)))

(define-cproc set-future-result! (gfuture::<graviton-future> result)
  ::<void>
  (set-future-result! gfuture result true))

(define-cproc set-future-exception! (gfuture::<graviton-future> exception error-message::<const-cstring>)
  ::<void>
  (let* ((conts SCM_NIL)
         (err?::bool false))
    (SDL_LockMutex (-> gfuture lock))
    (cond
      ((and (SCM_FALSEP (-> gfuture result)) (SCM_FALSEP (-> gfuture exception)))
       (let* ((msg::char* (malloc MAX_MESSAGE_LENGTH)))
         (bzero msg MAX_MESSAGE_LENGTH)
         (strncpy msg error-message (- MAX_MESSAGE_LENGTH 1))
         (set! (-> gfuture exception) exception
               (-> gfuture message) msg
               conts (-> gfuture continuations)
               (-> gfuture continuations) SCM_NIL)
         (SDL_CondSignal (-> gfuture cond))))
      (else
       (set! err? true)))
    (SDL_UnlockMutex (-> gfuture lock))
    (when err?
      (Scm_Error "result has been already set in <graviton-future>"))
    (unless (SCM_NULLP conts)
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (main-loop-apply cont (SCM_LIST2 SCM_FALSE exception)))
                conts))))

(define-cproc future-result&exception (gfuture::<graviton-future>)
  ::(<top> <top>)
  (let* ((result SCM_FALSE)
         (exception SCM_FALSE))
    (SDL_LockMutex (-> gfuture lock))
    (loop
     (set! result (-> gfuture result)
           exception (-> gfuture exception))
     (cond
       ((and (SCM_FALSEP result) (SCM_FALSEP exception))
        (SDL_CondWait (-> gfuture cond) (-> gfuture lock)))
       (else
        (break))))
    (set! (-> gfuture consumed?) true)
    (SDL_UnlockMutex (-> gfuture lock))
    (return result exception)))

(define-cproc add-future-continuation! (gfuture::<graviton-future> cont)
  ::<void>
  (let* ((result SCM_FALSE)
         (exception SCM_FALSE)
         (conts SCM_NIL))
    (SDL_LockMutex (-> gfuture lock))
    (set! conts (Scm_Cons cont (-> gfuture continuations))
          (-> gfuture continuations) conts
          result (-> gfuture result)
          exception (-> gfuture exception))
    (SDL_UnlockMutex (-> gfuture lock))
    (unless (and (SCM_FALSEP result) (SCM_FALSEP exception))
      (SDL_LockMutex (-> gfuture lock))
      (set! (-> gfuture consumed?) true
            (-> gfuture continuations) SCM_NIL)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (main-loop-apply cont (SCM_LIST2 result exception)))
                conts))))

(define-cproc on-main-thread? ()
  ::<boolean>
  (return (== main-thread-id (SDL_ThreadID))))

(define-cproc submit/main (thunk::<procedure>)
  ::<void>
  (main-loop-apply (SCM_OBJ thunk) SCM_NIL))

(define (submit/thread thunk)
  (thread-start! (make-thread
                   (lambda ()
                     (thunk)))))

(define (submit thunk type)
  (let1 %submit (case type
                  ((main) submit/main)
                  ((thread) submit/thread)
                  (else
                   (errorf "type must be 'main or 'thread, but got ~s" thread)))
    (%submit thunk)))

(define (force-future future)
  (receive (result exception) (future-result&exception future)
    (cond
      (result
       (apply values result))
      (exception
       (raise exception))
      (else
       (errorf "[BUG] result or exception must have a value.")))))

(define (%async-apply type proc args)
  (let ((args (apply list args))
        (future (make-future)))
    (submit (lambda ()
              (reset
                (guard (e (else (set-future-exception! future e (report-error e #f))))
                  (receive result (apply proc args)
                    (set-future-result! future result)))))
            type)
    future))

(define (async-apply proc :rest args)
  (%async-apply 'main proc args))

(define (async/thread-apply proc :rest args)
  (%async-apply 'thread proc args))

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
    (add-timer! sec (lambda ()
                      (submit/main cont)))))

(define (report-uncaught-future-exceptions messages)
  (for-each (lambda (msg)
              (display msg (current-error-port))
              (newline (current-error-port)))
            messages)
  (error "async failed, but the exception wasn't caught."))

(define (yield)
  (unless (on-main-thread?)
    (error "yield is unavailable on non-main thread."))
  (shift cont (submit/main cont)))

