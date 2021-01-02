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

  (export application-context
          make-application-context
          application-context-invalidate!
          lookup-application-context
          application-context-id
          application-context-slot-atomic-ref
          application-context-slot-atomic-update!
          application-context-slot-ref
          application-context-slot-set!
          define-application-context-slot

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

(define *application-context-slot-initial-forms* '())

(define application-context-next-id (make-id-generator))

(define *application-context-table-atom* (atom (make-hash-table 'equal?)))

(define-syntax define-application-context-slot
  (syntax-rules ()
    ((_ name vals ...)
     (push! *application-context-slot-initial-forms* (cons 'name (lambda () (list vals ...)))))))

(define-class <application-context> ()
  ((id :init-keyword :id)
   (mutex :init-form (make-mutex))
   (slot-table :init-form (make-hash-table 'eq?))))

(define application-context (make-parameter #f))

(define (make-application-context)
  (atomic *application-context-table-atom*
    (lambda (tbl)
      (define (make-id)
        (let loop ()
          (let1 id (number->string (random-integer #x10000000000000000) 36)
            (if (hash-table-contains? tbl id)
              (loop)
              id))))
      (rlet1 ctx (make <application-context> :id (make-id))
        (hash-table-put! tbl (~ ctx'id) ctx)))))

(define (application-context-invalidate! ctx)
  (atomic *application-context-table-atom*
    (lambda (tbl)
      (hash-table-delete! tbl (~ ctx'id)))))

(define (lookup-application-context id)
  (atomic *application-context-table-atom*
    (lambda (tbl)
      (hash-table-get tbl id #f))))

(define (application-context-id :optional ctx)
  (slot-ref (or ctx (application-context)) 'id))

(define (get-application-context-slot-ratom name)
  (with-slots (mutex slot-table) (application-context)
    (define (get-ratom)
      (or (hash-table-get slot-table name #f)
          (and-let1 thunk (assq-ref *application-context-slot-initial-forms* name #f)
            (rlet1 val-ratom (apply ratom (thunk))
              (hash-table-put! slot-table name val-ratom)))
          (errorf "application-context doesn't have such slot: ~a" name)))
    (cond
      ((eq? (mutex-state mutex) (current-thread))
       (get-ratom))
      (else
       (with-locking-mutex mutex get-ratom)))))

(define (application-context-slot-atomic-ref name proc)
  (unless (application-context)
    (errorf "application-context-slot-atomic-ref called without application-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-ratom (get-application-context-slot-ratom name)
    (ratomic val-ratom proc)))

(define (application-context-slot-ref name)
  (application-context-slot-atomic-ref name values))

(define (application-context-slot-atomic-update! name proc)
  (unless (application-context)
    (errorf "application-context-slot-atomic-update! called without application-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-ratom (get-application-context-slot-ratom name)
    (ratomic-update! val-ratom proc)))

(define (application-context-slot-set! name :rest vals)
  (application-context-slot-atomic-update! name (lambda _ (apply values vals))))

;;;

(define-application-context-slot control-out #f)

(define (app-exit code)
  (let1 out (application-context-slot-ref 'control-out)
    (write `(shutdown ,code) out)
    (flush out)))

