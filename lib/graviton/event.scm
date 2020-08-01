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
  (use gauche.regexp)
  (use gauche.threads)
  (use graviton.app)
  (use graviton.async)
  (use graviton.comm)
  (use graviton.config)
  (use graviton.jsbridge)
  (use util.match)

  (export <jsevent>
          add-jsevent-listener!
          remove-jsevent-listener!

          fire-event
          next-event
          all-events
          event-stream-closed?
          event-stream-close
          capture-jsevent
          set-frame-interval!
          frame-sync))

(select-module graviton.event)

(import-js ("graviton/event.mjs" :as Event))

(define-class <jsevent> ()
  ())

(define (js-property-slot-names+props event-class)
  (sort (fold (lambda (slot-def js-prop-slots)
                (match-let1 (slot-name . slot-opts) slot-def
                  (or (and-let1 prop (get-keyword :js-property slot-opts #f)
                        (acons slot-name prop js-prop-slots))
                      js-prop-slots)))
              '()
              (compute-slots event-class))
        (lambda (s1 s2)
          (string<? (cdr s1) (cdr s2)))))

(define (js-property-slot-names event-class)
  (map car (js-property-slot-names+props event-class)))

(define (js-property-slot-props event-class)
  (map cdr (js-property-slot-names+props event-class)))

(define-method initialize ((event <jsevent>) args)
  (next-method)
  (for-each (lambda (name val)
              (slot-set! event name val))
            (js-property-slot-names (class-of event))
            args))

(define (parse-prop str)
  (list->vector (reverse (fold (lambda (str result)
                                 (rxmatch-if (#/(.*)\[(\d+)\]/ str)
                                     (_ name index)
                                   (cons (string->number index) (cons name result))
                                   (cons str result)))
                               '()
                               (string-split str ".")))))

(define-application-context-slot listener-table (make-hash-table 'equal?))

(define (add-jsevent-listener! event-target event-type event-class-or-props proc)
  (let1 id (jsobject-id event-target)
    (application-context-slot-atomic-ref 'listener-table
      (lambda (tbl)
        (hash-table-push! tbl
                          (cons id event-type)
                          (list (if (is-a? event-class-or-props <class>)
                                    event-class-or-props
                                    #f)
                                proc))))
    (jslet ((event-target*::object* event-target)
            (event-type::json)
            (prop-vec::json (map-to <vector> parse-prop (cond
                                                          ((and (is-a? event-class-or-props <class>)
                                                                (member <jsevent> (class-precedence-list event-class-or-props)))
                                                           (js-property-slot-props event-class-or-props))
                                                          ((is-a? event-class-or-props <collection>)
                                                           event-class-or-props)
                                                          (else
                                                           (errorf "a class which inherits <jsevent> or a collection of property spec is required, but got ~s" event-class-or-props))))))
      (Event.registerEventHandler event-target* event-type prop-vec))))

(define (remove-jsevent-listener! event-target event-type proc)
  (let1 id (jsobject-id event-target)
    (application-context-slot-atomic-ref 'listener-table
      (lambda (tbl)
        (let1 key (cons jsobject-id event-type)
          (hash-table-update! tbl key (lambda (vals)
                                        (remove (match-lambda
                                                  ((_ handler)
                                                   (eq? proc handler)))
                                                vals))))))
    (jslet ((event-target*::object* event-target)
            (event-type::json))
      (Event.unregisterEventHandler event-target* event-type))))


(define-action "notifyEvent" (id event-type event-values)
  (let1 args (vector->list event-values)
    (for-each (match-lambda
                ((event-class proc)
                 (let1 args (if event-class
                                (list (apply make event-class args))
                                args)
                   (apply proc args))))
              (application-context-slot-atomic-ref 'listener-table
                (lambda (tbl)
                  (hash-table-get tbl (cons id event-type) '()))))))

;;;

(define-syntax symbol-macrolet
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ (and (((? symbol? sym) val) ...)
                 form-alist)
            body ...)
         (define (traverse expr)
           (match expr
             (('quote x)
              (list 'quote x))
             (('quasiquote x)
              (list 'quasiquote (traverse-quasiquote x)))
             ((e ...)
              (map traverse e))
             ((? symbol? sym)
              (let1 pair (assq sym form-alist)
                (if pair
                    (cadr pair)
                    sym)))
             (_
              expr)))
         (define (traverse-quasiquote expr)
           (match expr
             (('unquote x)
              (list 'unquote (traverse x)))
             ((e ...)
              (map traverse-quasiquote e))
             (_
              expr)))
         (quasirename rename `(begin ,@(traverse body))))
        (_
         (errorf "malformed symbol-macrolet: ~s" form))))))

(define-syntax with-slots
  (syntax-rules ()
    ((_ (slot ...) obj body ...)
     (symbol-macrolet ((slot (slot-ref obj 'slot)) ...) body ...))))

(define-class <event-stream> ()
  ((lock :init-form (make-mutex))
   (condition-variable :init-form (make-condition-variable))
   (priority-queue-table :init-form (make-hash-table 'eqv?))
   (state :init-value 'active)))

(define (make-event-stream)
  (make <event-stream>))

(define (sort-events events)
  (sort events (lambda (e1 e2)
                 (< (car e1) (car e2)))))

(define (event-stream-store! event-stream event-type args :optional (priority 0))
  (with-locking-mutex (slot-ref event-stream 'lock)
    (lambda ()
      (let* ((tbl (slot-ref event-stream 'priority-queue-table))
             (queue (hash-table-get tbl priority #f)))
        (unless queue
          (set! queue (make-queue))
          (hash-table-put! tbl priority queue))
        (enqueue! queue (list* event-type args))
        (condition-variable-broadcast! (slot-ref event-stream 'condition-variable))))))

(define (event-stream-fetch! event-stream)
  (with-slots (lock condition-variable priority-queue-table state) event-stream
    (mutex-lock! lock)
    (cond
      ((eq? state 'closed)
       (eof-object))
      (else
       (let loop ((priorities (sort (hash-table-keys priority-queue-table))))
         (cond
           ((and (not (null? priorities))
                 (dequeue! (hash-table-get priority-queue-table (car priorities)) #f))
            => (lambda (event)
                 (mutex-unlock! lock)
                 event))
           ((null? priorities)
            (mutex-unlock! lock condition-variable)
            (mutex-lock! lock)
            (loop (sort (hash-table-keys priority-queue-table))))
           (else
            (loop (cdr priorities)))))))))

(define (event-stream-fetch-all! event-stream :optional fallback)
  (with-slots (lock priority-queue-table state) event-stream
    (with-locking-mutex lock
      (lambda ()
        (cond
          ((eq? state 'closed)
           (if (undefined? fallback)
             (eof-object)
             fallback))
          (else
           (let loop ((priorities (sort (hash-table-keys priority-queue-table)))
                      (events '()))
             (cond
               ((null? priorities)
                events)
               (else
                (loop (cdr priorities)
                      (append events (dequeue-all! (hash-table-get priority-queue-table (car priorities))))))))))))))

(define (fire-event event-type args :optional (priority 0))
  (event-stream-store! (event-stream) event-type args priority))

(define (next-event)
  (event-stream-fetch! (event-stream)))

(define (all-events)
  (event-stream-fetch-all! (event-stream)))

(define (event-stream-closed?)
  (with-locking-mutex (slot-ref (event-stream) 'lock)
    (lambda ()
      (eq? (slot-ref (event-stream) 'state) 'closed))))

(define (event-stream-close)
  (with-locking-mutex (slot-ref (event-stream) 'lock)
    (lambda ()
      (slot-set! (event-stream) 'state 'closed))))

(define-application-context-slot event-stream (make-event-stream))

(define (event-stream)
  (application-context-slot-ref 'event-stream))

(define (capture-jsevent event-target event-type event-class-or-props :optional (priority 0))
  (let1 event-type-sym (string->symbol event-type)
    (add-jsevent-listener! event-target event-type event-class-or-props
      (lambda args
        (fire-event event-type-sym (cons event-target args) priority)))))

(define frame-interval-sec (/. 1 30))

(define (set-frame-interval! sec)
  (set! frame-interval-sec sec))

(define (frame-sync thunk)
  (let1 start-sec (time->seconds (current-time))
    (thunk)
    (let* ((end-sec (time->seconds (current-time)))
           (elapse (- end-sec start-sec)))
      (thread-sleep! (max 0 (- frame-interval-sec elapse))))))
