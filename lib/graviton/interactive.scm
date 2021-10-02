;;;
;;; interactive.scm - Utilities for interactive environment
;;;
;;;   Copyright (c) 2021 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.interactive
  (use gauche.hook)
  (use graviton)
  (use graviton.app)
  (use graviton.comm)
  (use graviton.misc)
  (use util.match)

  (export worker-eval
          worker-eval*
          worker-current-module
          worker-sandbox-module

          current-console

          print-main-worker-list))

(select-module graviton.interactive)

(define previous-window-context (make-window-parameter #f))

(define *eval-event* (gensym))
(define *query-worker-current-module-event* (gensym))
(define *query-worker-sandbox-module-event* (gensym))

(define vm-current-module (with-module gauche.internal vm-current-module))

(define sandbox-id-generator (make-id-generator))

(define %worker-current-module (make-window-parameter #f))
(define %worker-sandbox-module (make-window-parameter*
                                 (lambda ()
                                   (rlet1 sandbox (make-module (string->symbol (format "sandbox~a" (sandbox-id-generator))))
                                     (let1 original-module (vm-current-module)
                                       (for-each (lambda (module)
                                                   (eval `(import ,(module-name module)) sandbox))
                                                 (reverse (module-imports original-module)))
                                       (eval `(extend ,(module-name original-module)) sandbox))))))

(define current-console (make-parameter #f))

(define (inject-evaluator)
  (add-message-handler! *eval-event*
    (lambda (sexpr in out err trace console)
      (eval `(select-module ,(module-name (%worker-current-module))) (%worker-current-module))

      (guard (e (else (list 'error e)))
        (let1 vals (values->list (with-client-request
                                   (lambda ()
                                     (parameterize ((current-input-port (or in (current-input-port)))
                                                    (current-output-port (or out (current-output-port)))
                                                    (current-error-port (or err (current-error-port)))
                                                    (current-trace-port (or trace (current-trace-port)))
                                                    (current-console console))
                                       (eval sexpr (%worker-current-module))))))
          (%worker-current-module (vm-current-module))
          (list 'success vals)))))

  (add-message-handler! *query-worker-current-module-event*
    (lambda ()
      (%worker-current-module)))

  (add-message-handler! *query-worker-sandbox-module-event*
    (lambda ()
      (%worker-sandbox-module)))

  (%worker-current-module (%worker-sandbox-module)))

(add-hook! worker-start-hook inject-evaluator)

(define (worker-eval* sexpr worker success fail
                      :key
                      (input-port #f)
                      (output-port #f)
                      (error-port #f)
                      (trace-port #f)
                      (console #f))
  (unless (worker-active? worker)
    (errorf "~s is inactive" worker))

  (match ((worker *eval-event*
                  sexpr
                  input-port
                  output-port
                  error-port
                  trace-port
                  console))
    (('success vals)
     (apply success vals))
    (('error e)
     (fail e))))

(define (worker-eval sexpr worker
                     :key
                     (input-port #f)
                     (output-port #f)
                     (error-port #f)
                     (trace-port #f)
                     (console #f))
  (worker-eval* sexpr worker values raise
                :input-port input-port
                :output-port output-port
                :error-port error-port
                :trace-port trace-port
                :console console))

(define (worker-current-module worker)
  (unless (worker-active? worker)
    (errorf "~s is inactive" worker))

  ((worker *query-worker-current-module-event*)))

(define (worker-sandbox-module worker)
  (unless (worker-active? worker)
    (errorf "~s is inactive" worker))

  ((worker *query-worker-sandbox-module-event*)))

;;;

(define (window-context->main-worker ctx)
  (parameterize ((window-context ctx))
    (main-worker)))

(define (print-main-worker-list)
  (for-each (lambda (ctx)
              (let1 worker (window-context->main-worker ctx)
                (let-values (((title w h x y) (worker-eval '(values (~ document'title)
                                                                    (~ window'inner-width)
                                                                    (~ window'inner-height)
                                                                    (~ window'screen-left)
                                                                    (~ window'screen-top))
                                                           worker)))
                  (format #t
                          "(worker@ ~s)\t; ~s\t~ax~a+~a+~a~%"
                          (window-context-id ctx)
                          title
                          w
                          h
                          x
                          y))))
            (all-window-contexts))
  (undefined))

(define-in-module gauche (worker@ ctx-id)
  (let1 ctx (lookup-window-context ctx-id)
    (parameterize ((window-context ctx))
      (main-worker))))
