;;;
;;; misc.scm - Misc utilities
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

(define-module graviton.misc
  (use file.util)
  (use gauche.logger)
  (use gauche.threads)
  (use srfi-13)
  (use util.match)

  (export make-id-generator

          grv-log-config
          grv-access-log-drain
          grv-error-log-drain
          log-level
          log-framework-debug
          log-debug
          log-info
          log-error
          log-timestamp

          with-slots

          estimate-content-type

          now-seconds

          stat
          stat-time
          stat-memory))

(select-module graviton.misc)

;;;

(define (make-id-generator :optional (max-value #f) (start 0))
  (let1 counter-atom (atom start)
    (lambda ()
      (atomic-update! counter-atom (lambda (x)
                                     (if max-value
                                         (modulo (+ x 1) max-value)
                                         (+ x 1)))))))

;;;

(define-class <grv-config> ()
  ((port :init-value 0
         :init-keyword :port)
   (type :init-value 'player)
   (window-size :init-value '(800 600))
   (show? :init-value #t)
   (resizable? :init-value #f)))

(define-syntax set-config-param!
  (syntax-rules ()
    ((_ cfg)
     cfg)
    ((_ cfg param rest ...)
     (begin
       (unless (undefined? param)
         (slot-set! cfg 'param param))
       (set-config-param! cfg rest ...)))))

(define-syntax config-with-params
  (syntax-rules ()
    ((_ config-class param ...)
     (let1 cfg (make config-class)
       (set-config-param! cfg param ...)))))

;;;

(define-class <log-config> ()
  ((access-log-drain :init-value #f)
   (error-log-drain :init-value #t)
   (log-level :init-value 0)))

(define *log-config* (make <log-config>))

(define (grv-log-config :key
                        access-log-drain
                        error-log-drain
                        log-level)
  (set! *log-config* (config-with-params <log-config> access-log-drain error-log-drain log-level)))

(define (grv-access-log-drain)
  (slot-ref *log-config* 'access-log-drain))

(define (grv-error-log-drain)
  (slot-ref *log-config* 'error-log-drain))

(define (log-level)
  (slot-ref *log-config* 'log-level))

(define-constant DEBUG-LEVEL-FRAMEWORK 3)
(define-constant DEBUG-LEVEL 1)
(define-constant INFO-LEVEL 0)
(define-constant ERROR-LEVEL -1)

(define-syntax log-output
  (syntax-rules ()
    ((_ level fmt args ...)
     (when (<= level (log-level))
       (apply log-format fmt (list args ...))))))

(define-syntax log-framework-debug
  (syntax-rules ()
    ((_ fmt args ...)
     (log-output DEBUG-LEVEL-FRAMEWORK fmt args ...))))

(define-syntax log-debug
  (syntax-rules ()
    ((_ fmt args ...)
     (log-output DEBUG-LEVEL fmt args ...))))

(define-syntax log-info
  (syntax-rules ()
    ((_ fmt args ...)
     (log-output INFO-LEVEL fmt args ...))))

(define-syntax log-error
  (syntax-rules ()
    ((_ fmt args ...)
     (log-output INFO-LEVEL fmt args ...))))

(define-syntax log-timestamp
  (syntax-rules ()
    ((_ msg)
     (log-info "~a: ~:d ms" msg (round->exact (* 1000 (- (now-seconds) *start-timestamp*)))))
    ((_)
     (log-timestamp "timstamp"))))

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

;;;

(define (estimate-content-type filename)
  (let1 ext (string-downcase (path-extension filename))
    (cond
      ((member ext '("png"))
       "image/png")
      ((member ext '("jpg" "jpeg"))
       "image/jpeg")
      ((member ext '("gif"))
       "image/gif")
      ((member ext '("bmp"))
       "image/bmp")
      ((member ext '("mp3"))
       "audio/mpeg")
      ((member ext '("m4a"))
       "audio/aac")
      ((member ext '("ogg"))
       "audio/ogg")
      ((member ext '("mid" "midi"))
       "audio/midi")
      ((member ext '("wav"))
       "audio/wav")
      ((member ext '("js" "mjs"))
       "text/javascript")
      ((member ext '("css"))
       "text/css")
      (else
       #f))))

;;;

(define (now-seconds)
  (time->seconds (current-time)))

(define *start-timestamp* (now-seconds))

;;;

(define-class <value-metric> ()
  ((description :init-keyword :description
                :init-value "stat")
   (unit :init-keyword :unit
         :init-value "")
   (format-spec :init-keyword :format-spec
                :init-value "~f")
   (round-function :init-keyword :round-function
                   :init-value values)
   (mode :init-keyword :mode
         :init-value :absolute)
   (order-by :init-keyword :order-by
             :init-value :asc)
   (base-value :init-value 0)
   (value-list :init-value '())
   (additional-report :init-keyword :additional-report
                      :init-value (lambda (v) ""))
   (percentiles :init-keyword :percentiles
                :init-value '(0.5 0.75 0.9))))

(define (value-metric-set-base-value! metric v)
  (with-slots (base-value mode) metric
    (if (eq? mode :diff)
      (set! base-value v)
      (set! base-value 0))))

(define (value-metric-record! metric v num-samples)
  (with-slots (value-list base-value) metric
    (push! value-list (- v base-value))
    (when (<= num-samples (length value-list))
      (value-metric-report metric num-samples)
      (value-metric-clear! metric))))

(define (value-metric-report metric num-samples)
  (with-slots (description unit format-spec round-function value-list order-by additional-report percentiles) metric
    (define (report item v)
      (log-info #"~~a: ~format-spec ~~a ~~a" item (round-function v) unit (additional-report v)))
    (let* ((sorted-value-list (sort value-list (if (eq? order-by :desc) > <)))
           (len (length value-list)))
      (define (percentile p)
        (list-ref sorted-value-list (round->exact (* len p))))
      (log-info "*** ~a ***" description)
      (report "min" (apply min value-list))
      (report "avg" (/. (apply + value-list) len))
      (report "max" (apply max value-list))
      (dolist (p percentiles)
        (report (format "~d%ile" (round->exact (* p 100))) (percentile p))))))

(define (value-metric-clear! metric)
  (with-slots (base-value value-list) metric
    (set! base-value 0)
    (set! value-list '())))

(define-syntax stat
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ (specs ...) num-samples record-value body ...)
         (let1 metric (apply make <value-metric> specs)
           (quasirename rename
             `(begin0
                  (begin (when ,num-samples
                           (value-metric-set-base-value! ,metric ,record-value))
                         ,@body)
                (when ,num-samples
                  (value-metric-record! ,metric ,record-value ,num-samples))))))))))

(define *max-description-length* 50)

(define (make-description-string item sexpr)
  (let1 str (format "~a: ~s" item sexpr)
    (if (< (string-length str) *max-description-length*)
      str
      (string-append (substring str 0 *max-description-length*) "..."))))

(define-syntax stat-memory
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ title num-samples body ...)
         (quasirename rename
           `(stat (,:description ,(or title (make-description-string "memory" (car body)))
                                ,:format-spec "~:d"
                                ,:round-function ,round->exact
                                ,:unit "bytes"
                                ,:mode ,:diff)
                  ,num-samples
                  (car (assoc-ref (gc-stat) :total-bytes))
                  ,@body)))))))

(define-syntax stat-time
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ title num-samples body ...)
         (quasirename rename
           `(stat (,:description ,(or title (make-description-string "time" (car body)))
                                ,:format-spec "~,2,3f"
                                ,:unit "ms"
                                ,:mode ,:diff)
                  ,num-samples
                  (,now-seconds)
                  ,@body)))))))