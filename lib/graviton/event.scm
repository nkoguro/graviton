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
  (use control.thread-pool)
  (use file.util)
  (use gauche.collection)
  (use gauche.regexp)
  (use graviton)
  (use graviton.async)
  (use graviton.config)
  (use graviton.context)
  (use graviton.jsise)
  (use util.match)

  (export <jsevent>
          add-event-listener!
          remove-event-listener!))

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

(define (add-event-listener! event-target event-type event-class-or-props proc)
  (let1 proxy-id (proxy-id event-target)
    (application-context-slot-atomic-ref 'listener-table
      (lambda (tbl)
        (hash-table-push! tbl
                          (cons proxy-id event-type)
                          (list (current-thread-pool)
                                (if (is-a? event-class-or-props <class>)
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

(define (remove-event-listener! event-target event-type proc)
  (let1 proxy-id (proxy-id event-target)
    (application-context-slot-atomic-ref 'listener-table
      (lambda (tbl)
        (let1 key (cons proxy-id event-type)
          (hash-table-update! tbl key (lambda (vals)
                                        (remove (match-lambda
                                                  ((_ _ handler)
                                                   (eq? proc handler)))
                                                vals))))))
    (jslet ((event-target*::object* event-target)
            (event-type::json))
      (Event.unregisterEventHandler event-target* event-type))))


(define-action "notifyEvent" (id event-type event-values)
  (application-context-slot-atomic-ref 'listener-table
    (lambda (tbl)
      (let1 args (vector->list event-values)
        (for-each (match-lambda
                    ((pool event-class proc)
                     (let1 args (if event-class
                                    (list (apply make event-class args))
                                    args)
                       (guard (e (<thread-pool-shut-down> #f))
                         (submit-thunk pool (^() (apply proc args)))))))
                  (hash-table-get tbl (cons id event-type) '()))))))
