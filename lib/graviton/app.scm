;;;
;;; app.scm - Graviton application
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

(define-module graviton.app
  (use gauche.hook)
  (use gauche.parameter)
  (use gauche.selector)
  (use gauche.threads)
  (use graviton.misc)
  (use srfi-27)
  (use util.match)

  (export window-context
          make-window-context
          window-context-invalidate!
          lookup-window-context
          window-context-id
          window-context-slot-atomic-ref
          window-context-slot-atomic-update!
          window-context-slot-ref
          window-context-slot-set!
          all-window-contexts

          define-window-context-slot
          <window-parameter>
          make-window-parameter
          make-window-parameter*
          window-parameter-atomic-ref
          window-parameter-atomic-update!
          window-parameter-ref
          window-parameter-set!

          user-agent

          app-exit))

(select-module graviton.app)

;;;

(define-class <recursive-lock-atom> ()
  ((values :init-keyword :values)
   (mutex :init-form (make-mutex))))

(define (ratom :rest vals)
  (make <recursive-lock-atom> :values vals))

(define (ratom-lock! ratom)
  (with-slots (mutex) ratom
    (cond
      ((eq? (mutex-state mutex) (current-thread))
       (let1 n (mutex-specific mutex)
         (mutex-specific-set! mutex (+ n 1))))
      (else
       (mutex-lock! mutex)
       (mutex-specific-set! mutex 0)))))

(define (ratom-unlock! ratom)
  (with-slots (mutex) ratom
    (cond
      ((not (eq? (mutex-state mutex) (current-thread)))
       #f)
      (else
       (let1 n (mutex-specific mutex)
         (if (= n 0)
           (mutex-unlock! mutex)
           (mutex-specific-set! mutex (- n 1))))))))

;; Don't use dynamic-wind here.
;; The internal values may be changed after re-enter by calling the continuation in dynamic-wind case (shift unlocks the mutex).
;; It means atomicity is broken.
(define (with-locking-ratom ratom thunk)
  (unwind-protect
      (begin
        (ratom-lock! ratom)
        (thunk))
    (ratom-unlock! ratom)))

(define (ratomic ratom proc)
  (with-locking-ratom ratom
    (lambda ()
      (apply proc (slot-ref ratom 'values)))))

(define (ratomic-update! ratom proc)
  (with-locking-ratom ratom
    (lambda ()
      (receive vals (apply proc (slot-ref ratom 'values))
        (slot-set! ratom 'values vals)))))

;;;

(define *window-context-slot-initial-forms* '())

(define window-context-next-id (make-id-generator))

(define *window-context-table-atom* (atom (make-hash-table 'equal?)))

(define (register-window-context-slot! name init-proc)
  (push! *window-context-slot-initial-forms* (cons name init-proc)))

(define-class <window-parameter> ()
  ((key :init-form (gensym))))

(define (make-window-parameter* init-proc)
  (rlet1 wparam (make <window-parameter>)
    (register-window-context-slot! (slot-ref wparam 'key) (lambda ()
                                                            (values->list (init-proc))))))

(define (make-window-parameter :rest vals)
  (make-window-parameter* (^() (apply values vals))))

(define (window-parameter-atomic-ref ctx wparam proc)
  (window-context-slot-atomic-ref ctx (slot-ref wparam 'key) proc))

(define (window-parameter-atomic-update! ctx wparam proc)
  (window-context-slot-atomic-update! ctx (slot-ref wparam 'key) proc))

(define (window-parameter-ref ctx wparam)
  (window-parameter-atomic-ref ctx wparam values))

(define (window-parameter-set! ctx wparam :rest vals)
  (window-parameter-atomic-update! ctx wparam (^_ (apply values vals))))

(define-method object-apply ((wparam <window-parameter>) :rest args)
  (if (null? args)
    (window-parameter-ref (window-context) wparam)
    (apply window-parameter-set! (window-context) wparam args)))

(define-method (setter object-apply) ((wparam <window-parameter>) :rest vals)
  (apply wparam vals))

(define-syntax define-window-context-slot
  (syntax-rules ()
    ((_ name vals ...)
     (register-window-context-slot! 'name (lambda () (list vals ...))))))

(define-class <window-context> ()
  ((id :init-keyword :id)
   (mutex :init-form (make-mutex))
   (slot-table :init-form (make-hash-table 'eq?))))

(define %window-context (make-thread-local #f #t))

(define (window-context :rest args)
  (match args
    (()
     (tlref %window-context))
    ((ctx)
     (tlset! %window-context ctx))
    (else
     (errorf "Unexpected args for window-context: ~s" args))))

(define (make-window-context)
  (atomic *window-context-table-atom*
    (lambda (tbl)
      (define (make-id)
        (let loop ()
          (let1 id (number->string (random-integer #x10000000000000000) 36)
            (if (hash-table-contains? tbl id)
              (loop)
              id))))
      (rlet1 ctx (make <window-context> :id (make-id))
        (hash-table-put! tbl (slot-ref ctx 'id) ctx)))))

(define (window-context-invalidate! ctx)
  (atomic *window-context-table-atom*
    (lambda (tbl)
      (hash-table-delete! tbl (slot-ref ctx 'id)))))

(define (lookup-window-context id)
  (atomic *window-context-table-atom*
    (lambda (tbl)
      (hash-table-get tbl id #f))))

(define (window-context-id :optional (ctx #f))
  (slot-ref (or ctx (window-context)) 'id))

(define (get-window-context-slot-ratom ctx name)
  (with-slots (mutex slot-table) ctx
    (define (get-ratom)
      (or (hash-table-get slot-table name #f)
          (and-let1 thunk (assq-ref *window-context-slot-initial-forms* name #f)
            (rlet1 val-ratom (apply ratom (thunk))
              (hash-table-put! slot-table name val-ratom)))
          (errorf "window-context doesn't have such slot: ~a" name)))
    (cond
      ((eq? (mutex-state mutex) (current-thread))
       (get-ratom))
      (else
       (with-locking-mutex mutex get-ratom)))))

(define (window-context-slot-atomic-ref ctx name proc)
  (unless ctx
    (errorf "window-context-slot-atomic-ref called without window-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-ratom (get-window-context-slot-ratom ctx name)
    (ratomic val-ratom proc)))

(define (window-context-slot-ref ctx name)
  (window-context-slot-atomic-ref ctx name values))

(define (window-context-slot-atomic-update! ctx name proc)
  (unless ctx
    (errorf "window-context-slot-atomic-update! called without window-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-ratom (get-window-context-slot-ratom ctx name)
    (ratomic-update! val-ratom proc)))

(define (window-context-slot-set! ctx name :rest vals)
  (window-context-slot-atomic-update! ctx name (lambda _ (apply values vals))))

(define (all-window-contexts)
  (atomic *window-context-table-atom*
    (lambda (tbl)
      (hash-table-values tbl))))

(define-method ref ((ctx <window-context>) name)
  (window-context-slot-ref ctx name))

(define-method (setter ref) ((ctx <window-context>) name :rest vals)
  (apply window-context-slot-set! ctx name vals))

;;;

(define user-agent (make-window-parameter #f))

;;;

(define-window-context-slot control-out #f)

(define (app-exit code)
  (let1 out (window-context-slot-ref (window-context) 'control-out)
    (write `(shutdown ,code) out)
    (flush out)))

