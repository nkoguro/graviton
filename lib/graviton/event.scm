;;;
;;; event.scm - Event API
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

(define-module graviton.event
  (use data.queue)
  (use file.util)
  (use gauche.collection)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.regexp)
  (use gauche.threads)
  (use gauche.time)
  (use graviton.app)
  (use graviton.async)
  (use graviton.comm)
  (use graviton.config)
  (use graviton.jsffi)
  (use graviton.misc)
  (use srfi-13)
  (use util.match)

  (export <event>
          jsevent-callback-set!
          jsevent-callback-delete!
          on-jsevent
          request-animation-frame-callback!
          cancel-animation-frame-callback!
          on-repaint

          jsevent-wait
          ))

(select-module graviton.event)

(import-js ("/_g/event.mjs" :as Event))

(define-class <event> (<jsobject>)
  ((bubbles :jsproperty "bubbles"
            :read-only? #t)
   (cancel-bubble :jsproperty "cancelBubble")
   (cancelable :jsproperty "cancelable"
               :read-only? #t)
   (composed :jsproperty "composed"
             :read-only? #t)
   (current-target :jsproperty "currentTarget"
                   :read-only? #t)
   (default-prevented :jsproperty "defaultPrevented" :read-only? #t)
   (event-phase :jsproperty "eventPhase"
                :read-only? #t)
   (return-value :jsproperty "returnValue")
   (target :jsproperty "target"
           :read-only? #t)
   (timestamp :jsproperty "timestamp"
              :read-only? #t)
   (type :jsproperty "type"
         :read-only? #t)
   (is-trusted :jsproperty "isTrusted"
               :read-only? #t))
  :jsclass "Event")

(define-automatic-jsobject-methods <event>
  "composedPath")

(define-window-context-slot jsevent-callback-table (make-hash-table 'equal?))

(define (parse-prop-spec str)
  (list->vector (reverse (fold (lambda (str result)
                                 (rxmatch-if (#/(.*)\[(\d+)\]/ str)
                                     (_ name index)
                                   (cons (string->number index) (cons name result))
                                   (cons str result)))
                               '()
                               (string-split str ".")))))

(define-method jsevent-callback-set! ((jsobj <jsobject>) event-type prop-specs (callback <worker-callback>) :key (use-capture? #f))
  (jsevent-callback-delete! jsobj event-type)
  (window-context-slot-atomic-ref 'jsevent-callback-table
    (lambda (tbl)
      (hash-table-put! tbl (list jsobj event-type) callback)))
  (jslet ((obj::object jsobj)
          (event-type::string)
          (prop-specs)
          (callback)
          (use-capture?))
    (Event.registerEventHandler obj event-type prop-specs callback use-capture?))
  (undefined))

(define-method jsevent-callback-set! ((jsobj <jsobject>) event-type prop-specs (proc <procedure>) :key (use-capture? #f))
  (jsevent-callback-set! jsobj event-type prop-specs (worker-callback proc) :use-capture? use-capture?))

(define-method jsevent-callback-set! ((jsobj-provider <jsobject-provider>) event-type prop-specs proc-or-callback :key (use-capture? #f))
  (jsevent-callback-set! (jsobj-provider) event-type prop-specs proc-or-callback :use-capture? use-capture?))

(define-method jsevent-callback-delete! ((jsobj <jsobject>) event-type :key (use-capture? #f))
  (and-let1 callback (window-context-slot-atomic-ref 'jsevent-callback-table
                      (lambda (tbl)
                        (let1 key (list jsobj event-type)
                          (begin0
                              (hash-table-get tbl key #f)
                            (hash-table-delete! tbl key)))))
    (jslet ((obj::object jsobj)
            (event-type::string)
            (callback)
            (use-capture?))
      (Event.unregisterEventHandler obj event-type callback use-capture?))
    (unlink-callback callback)
    (undefined)))

(define-method jsevent-callback-delete! ((jsobj-provider <jsobject-provider>) event-type :key (use-capture? #f))
  (jsevent-callback-delete! (jsobj-provider) event-type :use-capture? use-capture?))

(define (estimate-prop-spec sym)
  (define (scm->camel-case str)
    (let1 parts (string-split str "-")
      (cond
        ((null? parts)
         "")
        (else
         (apply string-append (car parts) (map string-titlecase (cdr parts)))))))
  (let1 parts (string-split (symbol->string sym) ":")
    (list->vector (map (^s (regexp-replace #/\?$/ (scm->camel-case s) "")) parts))))

(define (parse-arg-specs arg-specs)
  (let loop ((specs arg-specs)
        (args '())
        (props '()))
    (match specs
      (()
       (values (reverse args) (list->vector (reverse props))))
      (((? symbol? arg) rest ...)
       (loop rest (cons arg args) (cons (estimate-prop-spec arg) props)))
      ((((? symbol? arg) (? string? prop-spec)) rest ...)
       (loop rest (cons arg args) (cons (parse-prop-spec prop-spec) props)))
      (_
       (errorf "malformed arg-specs: ~s" arg-specs)))))

(define-syntax on-jsevent
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ jsobj (type ':use-capture? use-capture?) (? list? arg-specs) body ...)
         (receive (args props) (parse-arg-specs arg-specs)
           (quasirename rename `(jsevent-callback-set! ,jsobj ,type ,props (lambda ,args ,@body) ,:use-capture? ,use-capture?))))
        ((_ jsobj type (? list? arg-specs) body ...)
         (receive (args props) (parse-arg-specs arg-specs)
           (quasirename rename `(jsevent-callback-set! ,jsobj ,type ,props (lambda ,args ,@body)))))
        ((_ jsobj (type ':use-capture? use-capture?) #f)
         (quasirename rename `(jsevent-callback-delete! ,jsobj ,type ,:use-capture? ,use-capture?)))
        ((_ jsobj type #f)
         (quasirename rename `(jsevent-callback-delete! ,jsobj ,type)))
        (_
         (errorf "malformed on-jsevent: ~s" form))))))

;; (define-syntax with-jsevents
;;   (er-macro-transformer
;;     (lambda (form rename id=?)
;;       (match form
;;         ((_ 
;; (with-jsevents ((window "keyup" (key) #=key)
;;                (window "keydown" (key) #?=key))
;;    aaa)

(define-window-context-slot animation-frame-callback #f)

(define-method request-animation-frame-callback! ((callback <worker-callback>))
  (let1 cur-callback (window-context-slot-ref 'animation-frame-callback)
    (cond
      ((and cur-callback (not (equal? cur-callback callback)))
       ;; Need to unlink the current callback.
       ;; Wait Graviton.requestAnimationFrameServerCallback intentionally to ensure removal of the current callback.
       (jslet/result ((callback))
         (result (Graviton.requestAnimationFrameServerCallback callback)))
       (unlink-callback cur-callback)
       (window-context-slot-set! 'animation-frame-callback callback))
      (else
       (jslet ((callback))
         (Graviton.requestAnimationFrameServerCallback callback))
       (unless cur-callback
         (window-context-slot-set! 'animation-frame-callback callback)))))
  (undefined))

(define-method request-animation-frame-callback! ((proc <procedure>))
  (animation-frame-callback-set! (worker-callback proc)))

(define (cancel-animation-frame-callback!)
  (let1 cur-callback (window-context-slot-ref 'animation-frame-callback)
    ;; Wait Graviton.requestAnimationFrameServerCallback intentionally to ensure removal of the current callback.
    (jslet/result ()
      (result (Graviton.requestAnimationFrameServerCallback undefined)))
    (when cur-callback
      (unlink-callback cur-callback))
    (window-context-slot-set! 'animation-frame-callback #f))
  (undefined))

(define-syntax on-repaint
  (syntax-rules (:priority)
    ((_ #f)
     (cancel-animation-frame-callback!))
    ((_ :priority priority (sec-per-frame) body ...)
     (let1 prev-time #f
       (letrec ((callback (worker-callback (lambda (cur-time)
                                             (let1 sec-per-frame (if prev-time
                                                                   (/. (- cur-time prev-time) 1000)
                                                                   0)
                                               body ...)
                                             (request-animation-frame-callback! callback)
                                             (set! prev-time cur-time))
                                           :priority priority)))
         (request-animation-frame-callback! callback))))
    ((_ (sec-per-frame) body ...)
     (on-repaint :priority #f (sec-per-frame) body ...))))

(define (jsevent-wait jsobj type :key (jsproperties #f) (timeout #f) (timeout-val #f) (use-capture? #f))
  (let ((worker (current-worker))
        (priority (current-priority))
        (returned? #f))
    (let1 v (shift cont
              (when timeout
                (let1 timeout-callback (worker-callback (^() (cont timeout-val)) :worker worker :priority priority)
                  (scheduler-add! timeout-callback :after timeout)))
              (letrec* ((event-callback (worker-callback (lambda (v)
                                                           (free-future-id future-id)
                                                           (cont v))
                                                         :worker worker
                                                         :priority priority))
                        (future-id (allocate-future-id event-callback)))
                (jslet ((jsobj::object)
                        (type::string)
                        (jsproperties::list)
                        (future-id)
                        (use-capture?))
                  (Event.registerOneShotEventHandler jsobj type jsproperties future-id use-capture?))))
      (cond
        (returned?
         (shift _
           ;; Escape from the current reset scope.
           #f))
        (else
         (set! returned? #t)
         v)))))
