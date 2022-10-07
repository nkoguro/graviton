;;;
;;; repl.scm - REPL
;;;
;;;   Copyright (c) 2022 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.repl
  (use gauche.threads)
  (use graviton.app)
  (use graviton.async)
  (use srfi-1)
  (use util.match)

  (export make-repl-group
          grv-repl
          next-repl
          list-repl
          select-repl))

(select-module graviton.repl)

(define %default-reader
  (let1 reader #f
    (lambda ()
      (unless reader
        (set! reader (or (and-let* ((m (find-module 'gauche.interactive))
                                    (reader (global-variable-ref m '%reader #f)))
                           (^() (parallel/await (reader))))
                         (^() (parallel/await (read))))))
      reader)))

(define %default-evaluator
  (let1 evaluator #f
    (lambda ()
      (unless evaluator
        (set! evaluator (or (and-let1 m (find-module 'gauche.interactive)
                              (global-variable-ref m '%evaluator #f))
                            eval)))
      evaluator)))

(define %default-printer
  (let1 printer #f
    (lambda ()
      (unless printer
        (set! printer (or (and-let1 m (find-module 'gauche.interactive)
                            (global-variable-ref m '%printer #f))
                          (lambda vals
                            (for-each (^e (write e) (newline)) vals)))))
      printer)))

(define %default-prompter
  (let1 prompter #f
    (lambda ()
      (unless prompter
        (set! prompter (or (and-let1 m (find-module 'gauche.interactive)
                             (global-variable-ref m '%prompter #f))
                           (^() (display "gosh> ") (flush)))))
      prompter)))

(define-class <grv-repl-group> ()
  ((repl-list-atom :init-form (atom '()))))

(define grv-default-repl-group (make <grv-repl-group>))

(define (make-repl-group)
  (make <grv-repl-group>))

(define-class <grv-repl> ()
  ((channel :init-form (make-channel))
   (worker :init-keyword :worker)
   (repl-group :init-keyword :repl-group)))

(define worker-repl (make-window-parameter #f))

(define (grv-repl :optional (reader #f) (evaluator #f) (printer #f) (prompter #f)
                  (repl-group grv-default-repl-group))
  (let ((reader (or reader (%default-reader)))
        (evaluator (or evaluator (%default-evaluator)))
        (printer (or printer (%default-printer)))
        (prompter (or prompter (%default-prompter))))
    (cond
      ((worker-repl)
       #f)
      (else
       (let1 repl (make <grv-repl> :repl-group repl-group :worker (current-worker))
         (worker-repl repl)
         (atomic-update! (~ repl-group'repl-list-atom)
           (lambda (lst)
             (append lst (list repl))))
         (unwind-protect
             (read-eval-print-loop
               reader
               evaluator
               printer
               (lambda ()
                 ;; Execute channel-send by grv-next-repl if exists.
                 (asleep 0)
                 (let loop ((active? #f))
                   (atomic (~ repl-group'repl-list-atom)
                     (lambda (lst)
                       (cond
                         ((null? lst)
                          (error "[bug] repl list is empty"))
                         ((eq? (first lst) repl)
                          (set! active? #t))
                         (else
                          #f))))
                   (cond
                     (active?
                      (prompter))
                     (else
                      (channel-recv (~ repl'channel))
                      (loop #f))))))
           (atomic-update! (~ repl-group'repl-list-atom)
             (lambda (lst)
               (let1 lst (delete repl lst eq?)
                 (unless (null? lst)
                   (concurrent
                     (channel-send (~ (first lst)'channel) #t)))
                 lst)))
           (worker-repl #f)))))))

(define (next-repl :optional (repl-group #f))
  (unless (worker-repl)
    (errorf "No repl exists for ~s." (current-worker)))

  (atomic-update! (~ (or repl-group (~ (worker-repl)'repl-group))'repl-list-atom)
    (lambda (lst)
      (cond
        ((or (null? lst) (= (length lst) 1))
         lst)
        (else
         (let1 new-lst (append (cdr lst) (list (car lst)))
           (concurrent
             (channel-send (~ (first new-lst)'channel) #t))
           new-lst))))))

(define (list-repl :optional (repl-group #f))
  (unless (worker-repl)
    (errorf "No repl exists for ~s." (current-worker)))

  (atomic (~ (or repl-group (~ (worker-repl)'repl-group))'repl-list-atom)
    (lambda (lst)
      lst)))

(define (select-repl repl)
  (unless (worker-repl)
    (errorf "No repl exists for ~s." (current-worker)))

  (let1 repl-group (~ (worker-repl)'repl-group)
  (atomic-update! (~ repl-group'repl-list-atom)
    (lambda (lst)
      (let loop ((rhead '())
                 (tail lst))
        (cond
          ((null? tail)
           (errorf "~s doesn't exist in ~s" repl repl-group))
          ((eq? repl (car tail))
           (let1 new-lst (append (list repl) (reverse rhead) (cdr tail))
             (concurrent
               (channel-send (~ repl'channel) #t))
             new-lst))
          (else
           (loop (cons (car tail) rhead) (cdr tail)))))))))
