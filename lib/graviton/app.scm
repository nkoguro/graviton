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
  (use util.match)

  (export application-context
          make-application-context
          application-context-id
          application-context-slot-atomic-ref
          application-context-slot-atomic-update!
          application-context-slot-ref
          application-context-slot-set!
          define-application-context-slot

          app-exit))

(select-module graviton.app)

(define *application-context-slot-initial-forms* '())

(define application-context-next-id (make-id-generator))

(define-syntax define-application-context-slot
  (syntax-rules ()
    ((_ name vals ...)
     (push! *application-context-slot-initial-forms* (cons 'name (lambda () (list vals ...)))))))

(define-class <application-context> ()
  ((id :init-form (application-context-next-id))
   (mutex :init-form (make-mutex))
   (slot-table :init-form (make-hash-table 'eq?))))

(define application-context (make-parameter #f))

(define (make-application-context)
  (make <application-context>))

(define (application-context-id)
  (slot-ref (application-context) 'id))

(define (get-application-context-slot-atom name)
  (with-slots (mutex slot-table) (application-context)
    (define (get-atom)
      (or (hash-table-get slot-table name #f)
          (and-let1 thunk (assq-ref *application-context-slot-initial-forms* name #f)
            (rlet1 val-atom (apply atom (thunk))
              (hash-table-put! slot-table name val-atom)))
          (errorf "application-context doesn't have such slot: ~a" name)))
    (cond
      ((eq? (mutex-state mutex) (current-thread))
       (get-atom))
      (else
       (with-locking-mutex mutex get-atom)))))

(define (application-context-slot-atomic-ref name proc)
  (unless (application-context)
    (errorf "application-context-slot-atomic-ref called without application-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-atom (get-application-context-slot-atom name)
    (atomic val-atom proc)))

(define (application-context-slot-ref name)
  (application-context-slot-atomic-ref name values))

(define (application-context-slot-atomic-update! name proc)
  (unless (application-context)
    (errorf "application-context-slot-atomic-update! called without application-context, name=~a, proc=~s, thread=~s" name proc (current-thread)))
  (let1 val-atom (get-application-context-slot-atom name)
    (atomic-update! val-atom proc)))

(define (application-context-slot-set! name :rest vals)
  (application-context-slot-atomic-update! name (lambda _ (apply values vals))))

;;;

(define-application-context-slot control-out #f)

(define (app-exit code)
  (let1 out (application-context-slot-ref 'control-out)
    (write `(shutdown ,code) out)
    (flush out)))

