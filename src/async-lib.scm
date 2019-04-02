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

(select-module graviton.async)

(inline-stub
 (declcode
  (.include "SDL.h"
            "gauche.h"
            "graviton.h"
            "stdbool.h"
            "strings.h")

  (.define MAX_MESSAGE_LENGTH 1024)
  )

 (define-cvar main-thread-id::SDL_threadID :static)

 (initcode
  (set! main-thread-id (SDL_ThreadID))
  )

 (define-cfn finalize-future (z data::void*)
   ::void :static
   (when (GRV_FUTUREP z)
     (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR z)))
       (SDL_DestroyCond (-> gfuture cond))
       (SDL_DestroyMutex (-> gfuture lock))
       (when (and (not (-> gfuture consumed)) (-> gfuture message))
         (GRV_NOTIFY_STACKTRACE (SCM_MAKE_STR_COPYING (-> gfuture message)))
         (free (-> gfuture message))))))

 (define-cfn Grv_MakeFuture ()
   (let* ((gfuture::GrvFuture* (SCM_NEW GrvFuture))
          (obj (GRV_FUTURE_BOX gfuture)))
     (set! (-> gfuture lock) (SDL_CreateMutex)
           (-> gfuture cond) (SDL_CreateCond)
           (-> gfuture result) SCM_FALSE
           (-> gfuture exception) SCM_FALSE
           (-> gfuture message) NULL
           (-> gfuture continuations) SCM_NIL
           (-> gfuture consumed) false)
     (Scm_RegisterFinalizer obj finalize-future NULL)
     (return obj)))

 (define-cfn Grv_SetFutureResult (gfuture::GrvFuture* result report-error?::bool)
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
          (set! (-> gfuture consumed) true)
          (SDL_UnlockMutex (-> gfuture lock))
          (for-each (lambda (cont)
                      (GRV_APPLY cont (SCM_LIST2 result SCM_FALSE)))
                    conts)))
       (report-error?
        (Scm_Error "result has been already set in <graviton-future>")))))

 (define-cfn await-callback (interval::Uint32 param::void*)
   ::Uint32 :static
   (let* ((cont (cast ScmObj param)))
     (GRV_APPLY cont SCM_NIL))
   (return 0))

 ) ;; end of inline-stub

(include "types.scm")

(define-cproc make-future ()
  (return (Grv_MakeFuture)))

(define-cproc set-future-result! (gfuture::<graviton-future> result)
  ::<void>
  (Grv_SetFutureResult gfuture result true))

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
      (set! (-> gfuture consumed) true)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (GRV_APPLY cont (SCM_LIST2 SCM_FALSE exception)))
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
    (set! (-> gfuture consumed) true)
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
      (set! (-> gfuture consumed) true
            (-> gfuture continuations) SCM_NIL)
      (SDL_UnlockMutex (-> gfuture lock))
      (for-each (lambda (cont)
                  (GRV_APPLY cont (SCM_LIST2 result exception)))
                conts))))

(define-cproc on-main-thread? ()
  ::<boolean>
  (return (== main-thread-id (SDL_ThreadID))))

(define-cproc submit/main (thunk::<procedure>)
  ::<void>
  (GRV_APPLY (SCM_OBJ thunk) SCM_NIL))

(define-cproc add-cont-timer! (sec::<double> cont::<procedure>)
  ::<void>
  (let* ((interval::Uint32 (cast Uint32 (* sec 1000))))
    (SDL_AddTimer interval await-callback cont)))
