;;;
;;; graviton2.scm - Graviton
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

(define-module graviton2
  (use binary.io)
  (use control.thread-pool)
  (use data.queue)
  (use file.util)
  (use gauche.config)
  (use gauche.hook)
  (use gauche.logger)
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.process)
  (use gauche.regexp)
  (use gauche.selector)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.time)
  (use gauche.uvector)
  (use graviton2.jsise)
  (use makiki)
  (use rfc.base64)
  (use rfc.json)
  (use rfc.sha)
  (use scheme.list)
  (use srfi-13)
  (use srfi-19)
  (use srfi-27)
  (use srfi-42)
  (use srfi-98)
  (use text.html-lite)
  (use text.tree)
  (use util.match)

  (export <text-metrics>
          set-graviton-title!
          set-graviton-port!
          set-graviton-error-log-drain!
          set-graviton-use-player!
          set-graviton-background-color!
          set-graviton-open-dev-tools!
          grv-main
          grv-begin

          async-apply-main
          async-main
          await
          await-window-event
          await-canvas-event
          asleep

          flush-commands
          app-close
          window-size
          make-canvas
          load-canvas
          current-canvas
          set-canvas-visible!
          unlink-proxy-object!

          linear-gradient
          radial-gradient
          pattern
          current-fill-style
          set-fill-style!
          current-font
          set-font!
          current-global-alpha
          set-global-alpha!
          current-global-composite-operation
          set-global-composite-operation!
          current-image-smoothing-enabled?
          set-image-smoothing-enabled!
          current-line-cap
          set-line-cap!
          current-line-dash
          set-line-dash!
          current-line-dash-offset
          set-line-dash-offset!
          current-line-join
          set-line-join!
          current-line-width
          set-line-width!
          current-miter-limit
          set-miter-limit!
          current-shadow-blur
          set-shadow-blur!
          current-shadow-color
          set-shadow-color!
          current-shadow-offset-x
          set-shadow-offset-x!
          current-shadow-offset-y
          set-shadow-offset-y!
          current-stroke-style
          set-stroke-style!
          current-text-align
          set-text-align!
          current-text-baseline
          set-text-baseline!

          arc
          arc-to
          begin-path
          bezier-curve-to
          clear-rect
          clip
          close-path
          draw-canvas
          ellipse
          fill
          fill-rect
          fill-text
          get-image-data
          is-point-in-path?
          is-point-in-stroke?
          line-to
          measure-text
          move-to
          put-image-data
          quadratic-curve-to
          rect
          restore-context
          rotate
          save-context
          scale
          set-transform!
          stroke
          stroke-rect
          stroke-text
          transform
          translate
          create-image-data
          upload-image-data
          download-image-data

          loop-frame

          set-window-event-handler!
          set-canvas-event-handler!

          load-audio
          play-audio
          pause-audio
          play-wave
          play-rest
          load-pcm
          play-pcm
          ))

(select-module graviton2)

;;;

(define is-installed?
  (and (current-load-path)
       (equal? (gauche-site-library-directory) (sys-dirname (current-load-path)))))

(define *graviton-js-pathname* (build-path (if is-installed?
                                               (gauche-architecture-directory)
                                               ".")
                                           "graviton.js"))

;;;

(define (make-id-generator :optional (max-value #f) (start 0))
  (let1 counter-atom (atom start)
    (lambda ()
      (atomic-update! counter-atom (lambda (x)
                                     (if max-value
                                         (modulo (+ x 1) max-value)
                                         (+ x 1)))))))

;;;

(define (make-pool-thunk app-context pool thunk :optional (error-handler #f))
  (lambda ()
    (reset
      (parameterize ((application-context app-context)
                     (current-thread-pool pool))
        (guard (e (else
                   (cond
                     (error-handler
                      (error-handler e))
                     (else
                      (report-error e)
                      (exit 70)))))
          (thunk))))))

(define (submit-thunk pool thunk :optional (error-handler #f))
  (let1 app-context (application-context)
    (add-job! pool (make-pool-thunk app-context pool thunk error-handler))))

(define (add-event-listener! event-type proc-or-future)
  (let ((app-context (application-context))
        (pool (current-thread-pool)))
    (atomic (slot-ref app-context 'listener-table-atom)
      (lambda (tbl)
        (hash-table-push! tbl
                          event-type
                          (cond
                            ((procedure? proc-or-future)
                             (list pool proc-or-future))
                            ((is-a? proc-or-future <graviton-future>)
                             proc-or-future)
                            (else
                             (errorf "<procedure> or <graviton-future> required, but got ~s" proc-or-future))))))))

(define (delete-event-listener! event-type proc-or-future)
  (atomic (slot-ref (application-context) 'listener-table-atom)
    (lambda (tbl)
      (let loop ((vals '())
                 (rest (hash-table-get tbl event-type)))
        (match rest
          (()
           (hash-table-put! tbl event-type vals))
          (((_ (? (cut equal? proc-or-future <>) proc)) rest ...)
           (loop vals rest))
          (((? (cut equal? proc-or-future <>)) rest ...)
           (loop vals rest))
          ((v rest ...)
           (loop (cons v vals) rest)))))))

(define (invoke-event-handler event-type :rest args)
  (atomic (slot-ref (application-context) 'listener-table-atom)
    (lambda (tbl)
      (let1 vals (hash-table-get tbl event-type '())
        (hash-table-put! tbl
                         event-type
                         (fold (lambda (val new-vals)
                                 (match val
                                   ((pool proc)
                                    (add-job! pool (make-pool-thunk (application-context)
                                                       pool
                                                     (lambda ()
                                                       (apply proc args))))
                                    (cons val new-vals))
                                   (future
                                    (set-future-result-values&exception! future args #f)
                                    new-vals)))
                               '()
                               (hash-table-get tbl event-type '())))))))

;;;

;; future is await-able only in the main thread.
(define-class <graviton-future> ()
  ((lock :init-form (make-mutex))
   (result-maker :init-value values
                 :init-keyword :result-maker)
   (result-values :init-value #f)
   (result-exception :init-value #f)
   (pool&continuations :init-value '())))

(define (set-future-result-values&exception! future args exception)
  (let ((lock (slot-ref future 'lock))
        (pool&conts '())
        (vals (values->list (apply (slot-ref future 'result-maker) args))))
    (unwind-protect
        (begin
          (mutex-lock! lock)
          (cond
            ((or (slot-ref future 'result-values)
                 (slot-ref future 'result-exception))
             ;; If values or exception is set, do nothing.
             #f)
            (else
             (slot-set! future 'result-values vals)
             (slot-set! future 'result-exception exception)
             (set! pool&conts (slot-ref future 'pool&continuations))
             (slot-set! future 'pool&continuations '()))))
      (mutex-unlock! lock))
    (for-each (match-lambda ((pool cont)
                             (add-job! pool
                               (lambda ()
                                 (reset
                                   (cont vals exception))))))
              pool&conts)))

(define (await future :optional (timeout #f) :rest timeout-vals)
  (let ((lock (slot-ref future 'lock))
        (result-values #f)
        (result-exception #f))
    (mutex-lock! lock)
    (cond
      ((slot-ref future 'result-values)
       => (lambda (vals)
            (set! result-values vals)
            (mutex-unlock! lock)))
      ((slot-ref future 'result-exception)
       => (lambda (exception)
            (set! result-exception exception)
            (mutex-unlock! lock)))
      (else
       (let1 pool (current-thread-pool)
         (receive (vals exception) (shift cont
                                     (push! (slot-ref future 'pool&continuations) (list pool cont))
                                     (when timeout
                                       (add-timeout! timeout future timeout-vals))
                                     (mutex-unlock! lock))
           (set! result-values vals)
           (set! result-exception exception)))))
    (cond
      (result-values
       (if (null? result-values)
           (undefined)
           (apply values result-values)))
      (result-exception
       (raise result-exception))
      (else
       (error "[BUG] Invalid future state")))))

(define (await-event event-type :optional (timeout #f) :rest timeout-vals)
  (let1 future (make <graviton-future>)
    (add-event-listener! event-type future)
    (apply await future timeout timeout-vals)))

(define (await-window-event event-type :optional (timeout #f) :rest timeout-vals)
  (apply await-event event-type timeout timeout-vals))

(define (await-canvas-event canvas event-type :optional (timeout #f) :rest timeout-vals)
  (apply await-event (canvas-event-type canvas event-type) timeout timeout-vals))

(define (asleep sec)
  (let1 future (make <graviton-future>)
    (await future sec)))

(define (async-apply-main proc args)
  (let ((future (make <graviton-future>))
        (app-context (application-context)))
    (submit-thunk (main-thread-pool)
                  (lambda ()
                    (set! result-values (values->list (apply proc args))))
                  (lambda (e)
                    (set! result-exception e)))
    future))

(define-syntax async-main
  (syntax-rules ()
    ((_ expr ...)
     (async-apply-main (lambda () expr ...) '()))))

(define (register-future! future)
  (with-future-table
    (lambda (tbl future-next-id)
      (define (loop)
        (let1 id (future-next-id)
          (cond
            ((hash-table-contains? tbl id)
             (loop))
            (else
             (hash-table-put! tbl id future)
             id))))
      (loop))))

(define (notify-result id vals err)
  (with-future-table
    (lambda (tbl future-next-id)
      (let1 future (hash-table-get tbl id #f)
        (cond
          (future
           (set-future-result-values&exception! future
                                                (if (eq? vals 'false)
                                                    #f
                                                    (vector->list vals))
                                                (if (eq? err 'false)
                                                    #f
                                                    (condition
                                                      (&message (message err))
                                                      (&error))))
           (hash-table-delete! tbl id))
          ((not (eq? err 'false))
           (raise err))
          (else
           (errorf "[BUG] Invalid future ID: ~a" id)))))))

(define (notify-binary-result id)
  (with-future-table
    (lambda (tbl future-next-id)
      (let1 future (hash-table-get tbl id #f)
        (cond
          (future
           (let1 data (atomic *binary-data-table-atom*
                        (lambda (tbl)
                          (begin0
                            (hash-table-get tbl id #f)
                            (hash-table-delete! tbl id))))
             (unless data
               (errorf "[BUG] No binary data found for ID: ~a" id))
             (set-future-result-values&exception! future (list data) #f))
           (hash-table-delete! tbl id))
          (else
           (errorf "[BUG] Invalid future ID: ~a" id)))))))

(define-class <key-event> ()
  ((alt? :init-keyword :alt?)
   (code :init-keyword :code)
   (ctrl? :init-keyword :ctrl?)
   (composing? :init-keyword :composing?)
   (key :init-keyword :key)
   (locale :init-keyword :locale)
   (location :init-keyword :location)
   (meta? :init-keyword :meta?)
   (repeat? :init-keyword :repeat?)
   (shift? :init-keyword :shift?)))

(define-class <mouse-event> ()
  ((alt? :init-keyword :alt?)
   (button :init-keyword :button)
   (buttons :init-keyword :buttons)
   (client-x :init-keyword :client-x)
   (client-y :init-keyword :client-y)
   (ctrl? :init-keyword :ctrl?)
   (meta? :init-keyword :meta?)
   (movement-x :init-keyword :movement-x)
   (movement-y :init-keyword :movement-y)
   (offset-x :init-keyword :offset-x)
   (offset-y :init-keyword :offset-y)
   (screen-x :init-keyword :screen-x)
   (screen-y :init-keyword :screen-y)
   (shift? :init-keyword :shift?)
   (canvas-x :init-keyword :canvas-x)
   (canvas-y :init-keyword :canvas-y)))

(define-class <wheel-event> (<mouse-event>)
  ((delta-x :init-keyword :delta-x)
   (delta-y :init-keyword :delta-y)
   (delta-z :init-keyword :delta-z)
   (delta-mode :init-keyword :delta-mode)))

(define (alist->event alist)
  (let1 event-type (assoc-ref alist "type")
    (cond
      ((equal? event-type "KeyboardEvent")
       (make <key-event>
         :alt? (assoc-ref alist "altKey" #f)
         :code (assoc-ref alist "code" #f)
         :ctrl? (assoc-ref alist "ctrlKey" #f)
         :composing? (assoc-ref alist "isComposing" #f)
         :key (assoc-ref alist "key" #f)
         :locale (assoc-ref alist "locale" #f)
         :meta? (assoc-ref alist "metaKey" #f)
         :repeat? (assoc-ref alist "repeat" #f)
         :shift? (assoc-ref alist "shiftKey" #f)))
      ((equal? event-type "MouseEvent")
       (make <mouse-event>
         :alt? (assoc-ref alist "altKey" #f)
         :button (assoc-ref alist "button" #f)
         :buttons (assoc-ref alist "buttons" #())
         :client-x (assoc-ref alist "clientX" #f)
         :client-y (assoc-ref alist "clientY" #f)
         :ctrl? (assoc-ref alist "ctrlKey" #f)
         :meta? (assoc-ref alist "metaKey" #f)
         :movement-x (assoc-ref alist "movementX" #f)
         :movement-y (assoc-ref alist "movementY" #f)
         :offset-x (assoc-ref alist "offsetX" #f)
         :offset-y (assoc-ref alist "offsetY" #f)
         :screen-x (assoc-ref alist "screenX" #f)
         :screen-y (assoc-ref alist "screenY" #f)
         :shift? (assoc-ref alist "shiftKey" #f)
         :canvas-x (assoc-ref alist "canvasX" #f)
         :canvas-y (assoc-ref alist "canvasY" #f)))
      ((equal? event-type "WheelEvent")
       (make <wheel-event>
         :alt? (assoc-ref alist "altKey" #f)
         :button (assoc-ref alist "button" #f)
         :buttons (assoc-ref alist "buttons" #f)
         :client-x (assoc-ref alist "clientX" #f)
         :client-y (assoc-ref alist "clientY" #f)
         :ctrl? (assoc-ref alist "ctrlKey" #f)
         :meta? (assoc-ref alist "metaKey" #f)
         :movement-x (assoc-ref alist "movementX" #f)
         :movement-y (assoc-ref alist "movementY" #f)
         :offset-x (assoc-ref alist "offsetX" #f)
         :offset-y (assoc-ref alist "offsetY" #f)
         :screen-x (assoc-ref alist "screenX" #f)
         :screen-y (assoc-ref alist "screenY" #f)
         :shift? (assoc-ref alist "shiftKey" #f)
         :canvas-x (assoc-ref alist "canvasX" #f)
         :canvas-y (assoc-ref alist "canvasY" #f)
         :delta-x (assoc-ref alist "deltaX" #f)
         :delta-y (assoc-ref alist "deltaY" #f)
         :delta-z (assoc-ref alist "deltaZ" #f)
         :delta-mode (assoc-ref alist "deltaMode" #f)))
      (else
       (errorf "Unsupported event: ~a" event-type)))))

(define (notify-client-event event-type-str event-alist)
  (invoke-event-handler (string->symbol event-type-str) (alist->event event-alist)))

;;;

(define *scheduler-command-queue* (make-mtqueue))

(define (run-scheduler)
  (thread-start! (make-thread
                   (lambda ()
                     (guard (e (else (report-error e)
                                     (exit 70)))
                       (let loop ((schedule-list '()))
                         (let1 now (current-time)
                           (cond
                             ((and (not (null? schedule-list))
                                   (time<=? (caar schedule-list) now))
                              (match-let1 (_ future vals) (car schedule-list)
                                (set-future-result-values&exception! future vals #f))
                              (loop (cdr schedule-list)))
                             (else
                              (let1 timeout (if (null? schedule-list)
                                                #f
                                                (max (- (time->seconds (caar schedule-list))
                                                        (time->seconds now))
                                                     0))
                                (match (dequeue/wait! *scheduler-command-queue* timeout #f)
                                  (('shutdown)
                                   #f)
                                  (('schedule wake-time future vals)
                                   (loop (sort (cons (list wake-time future vals) schedule-list)
                                               time<?
                                               car)))
                                  (('cancel future)
                                   (loop (remove (lambda (schedule)
                                                   (eq? (cdr schedule) future))
                                                 schedule-list)))
                                  (_
                                   (loop schedule-list))))))))))
                   "scheduler")))

(define (shutdown-scheduler!)
  (enqueue! *scheduler-command-queue* '(shutdown)))

(define (add-schedule! wake-time future vals)
  (enqueue! *scheduler-command-queue* (list 'schedule wake-time future vals)))

(define (make-time-from-second type sec)
  (let* ((sec-part (floor sec))
         (nanosec-part (round->exact (* (- sec sec-part)
                                        1000000000))))
    (make-time type nanosec-part sec-part)))

(define (add-timeout! timeout-in-sec future vals)
  (add-schedule! (add-duration (current-time)
                               (make-time-from-second time-duration timeout-in-sec))
                 future
                 vals))

(define (cancel-schedule! future)
  (enqueue! *scheduler-command-queue* (list 'cancel future)))

;;;

(define-class <proxy-object> ()
  ((id :init-value #f)))

(define-method initialize ((obj <proxy-object>) :rest args)
  (next-method)
  (slot-set! obj 'id (proxy-object-next-id)))

(define (proxy-object-id obj)
  (or (slot-ref obj 'id)
      (errorf "~s was unlinked" obj)))

(define (unlink-proxy-object! obj)
  (cond
    ((slot-ref obj 'id) => (lambda (id)
                             (slot-set! obj 'id #f)
                             (jslet (id::u32)
                               (unlinkProxyObject id))))
    (else
     #t)))

;;;

(log-open #t)

(define (log-debug fmt :rest args)
  (apply log-format fmt args))

(define (log-info fmt :rest args)
  (apply log-format fmt args))

(define (log-error fmt :rest args)
  (apply log-format fmt args))

;;;

(define *title* #f)

(define (set-graviton-title! title)
  (set! *title* title))

(define *background-color* "#FFF")

(define (set-graviton-background-color! color)
  (set! *background-color* color))

(define-http-handler "/"
  (lambda (req app)
    (let1 title (or *title*
                    (if (and (list? (command-line))
                             (<= (length (command-line)) 1))
                        (path-sans-extension (sys-basename (list-ref (command-line) 0)))
                        "Graviton"))
      (respond/ok req (tree->string
                        (html:html
                         (html:head
                          (html:meta :charset "UTF-8")
                          (html:title title))
                         (apply html:body :style (format "background-color: ~a" *background-color*)
                                (html:div :id "_on")
                                (html:script :src "graviton.js")
                                (map (lambda (js-mod)
                                       (html:script :src (format "js/~a" js-mod)))
                                     (js-module-list)))))))))

(define-http-handler "/graviton.js"
  (lambda (req app)
    (respond/ok req `(file ,*graviton-js-pathname*))))

(define-http-handler #/js\/(.*)/
  (lambda (req app)
    (respond/ok req
      (get-js-code ((slot-ref req 'path-rxmatch) 1))
      :content-type "text/javascript")))

(define-class <websocket-server-context> ()
  ((continuation-opcode :init-value 0)
   (continuation-frames :init-value '())))

(define (reset-context! ctx)
  (slot-set! ctx 'continuation-opcode 0)
  (slot-set! ctx 'continuation-frames '()))

(define (send-frame out opcode payload)
  (let1 payload-len (u8vector-length payload)
    (write-u8 (logior #x80 opcode) out)
    (write-u8 (cond
                ((< payload-len 126)
                 payload-len)
                ((< payload-len #xffff)
                 126)
                (else
                 127))
              out)
    ;; Extended payload
    (cond
      ((< payload-len 126)
       #f)
      ((< payload-len #xffff)
       (write-u16 payload-len out 'big-endian))
      (else
       (write-u64 payload-len out 'big-endian)))
    (write-uvector payload out)
    (flush out)))

(define (send-text-frame out text)
  (send-frame out 1 (string->u8vector text)))

(define (send-binary-frame out data)
  (send-frame out 2 data))

(define (dispatch-payload ctx
                          in
                          out
                          :key
                          (text-handler #f)
                          (binary-handler #f)
                          (close-handler #f)
                          (pong-handler #f))
  (let1 data (read-u16 in 'big-endian)
    (cond
      ((eof-object? data)
       (close-input-port in))
      (else
       (let* ((fin? (logbit? 15 data))
              (opcode (logand (ash data -8) #b1111))
              (mask? (logbit? 7 data))
              (payload-length
               (let1 v (logand data #b1111111)
                 (cond
                   ((< v 126)
                    v)
                   ((= v 126)
                    (read-u16 in 'big-endian))
                   ((= v 127)
                    (read-u64 in 'big-endian)))))
              (masking-key (if mask?
                               (read-uvector <u8vector> 4 in)
                               #u8(0 0 0 0)))
              (payload-data (let1 uvec (make-u8vector payload-length)
                              (dotimes (i payload-length)
                                (u8vector-set! uvec
                                               i
                                               (logxor (read-u8 in)
                                                       (u8vector-ref masking-key (modulo i 4)))))
                              uvec)))
         (log-debug "receive frame: fin?=~a, opcode=~a, payload-length=~a" fin? opcode payload-length)
         (case opcode
           ((0)
            (cond
              (fin?
               (let ((cont-opcode (slot-ref ctx 'continuation-opcode))
                     (cont-frames (cons payload-data (slot-ref ctx 'continuation-frames))))
                 (reset-context! ctx)
                 (case (slot-ref ctx 'continuation-opcode)
                   ((1)
                    (text-handler (u8vector->string (apply u8vector-append (reverse cont-frames))))))
                 ((2)
                  (binary-handler (apply u8vector-append (reverse cont-frames))))))
              (else
               (push! (slot-ref ctx 'continuation-frames) payload-data))))
           ((1)
            (cond
              (fin?
               (text-handler (u8vector->string payload-data)))
              (else
               (slot-set! ctx 'continuation-opcode opcode)
               (slot-set! ctx 'continuation-frames (list payload-data)))))
           ((2)
            (cond
              (fin?
               (binary-handler payload-data))
              (else
               (slot-set! ctx 'continuation-opcode opcode)
               (slot-set! ctx 'continuation-frames (list payload-data)))))
           ((8)
            (when close-handler
              (let ((status (get-u16 payload-data 0 'big-endian))
                    (rest (uvector-alias <u8vector> payload-data 2 (u8vector-length payload-data))))
                (close-handler status rest))))
           ((9)
            (log-debug "Received ping frame: ~s" payload-data)
            (send-frame out #xa payload-data))
           ((8)
            (when pong-handler
              (pong-handler payload-data)))))))))

(define *initial-thunk* #f)
(define *num-dispatcher* (atom 0))
(define *init-hook* (make-hook))

(define-class <send-context> ()
  ((websocket-output-port :init-keyword :websocket-output-port)
   (buffer-output-port :init-keyword :buffer-output-port)
   (json-pair-buffer :init-value '())
   (json-next-id-generator :init-form (make-id-generator #xffffffff))
   (proxy-object-id-generator :init-form (make-id-generator #xffffffff))
   (refresh-cycle-second :init-value (/. 1 30))
   (next-update-time :init-form (current-time))))

(define-class <application-context> ()
  ((main-thread-pool :init-keyword :main-thread-pool)
   (worker-thread-pool :init-keyword :worker-thread-pool)
   (send-context-atom :init-keyword :send-context-atom)
   (listener-table-atom :init-form (atom (make-hash-table 'equal?)))
   (future-table-atom :init-form (atom (make-hash-table 'equal?) (make-id-generator #xffffffff)))))

(define application-context (make-parameter #f))
(define current-thread-pool (make-parameter #f))

(define (make-application-context num-main-pool-size num-worker-pool-size websocket-output-port)
  (let* ((send-context (make <send-context>
                         :websocket-output-port websocket-output-port
                         :buffer-output-port (open-output-uvector)))
         (app-context (make <application-context>
                        :main-thread-pool (make-thread-pool num-main-pool-size)
                        :worker-thread-pool (make-thread-pool num-worker-pool-size)
                        :send-context-atom (atom send-context))))
    app-context))

(define (main-thread-pool)
  (slot-ref (application-context) 'main-thread-pool))

(define (worker-thread-pool)
  (slot-ref (application-context) 'worker-thread-pool))

(define current-send-context (make-parameter #f))

(define (with-send-context proc)
  (cond
    ((current-send-context)
     (proc (current-send-context)))
    (else
     (atomic (slot-ref (application-context) 'send-context-atom)
       (lambda (ctx)
         (parameterize ((current-send-context ctx))
           (proc ctx)))))))

(define (refresh-cycle-second)
  (with-send-context
    (lambda (ctx)
      (slot-ref ctx 'refresh-cycle-second))))

(define (next-update-second)
  (with-send-context
    (lambda (ctx)
      (time->seconds (slot-ref ctx 'next-update-time)))))

(define (set-next-update-time! next-update-time)
  (with-send-context
    (lambda (ctx)
      (slot-set! ctx 'next-update-time next-update-time))))

(define (proxy-object-next-id)
  (with-send-context (lambda (ctx)
                       ((slot-ref ctx 'proxy-object-id-generator)))))

(define (json-next-id)
  (with-send-context (lambda (ctx)
                       ((slot-ref ctx 'json-next-id-generator)))))

(define (with-future-table proc)
  (atomic (slot-ref (application-context) 'future-table-atom) proc))

(define (start-application)
  (when *initial-thunk*
    (submit-thunk (main-thread-pool)
      (lambda ()
        (run-hook *init-hook*)
        (*initial-thunk*)))))

(define json-command-table
  (alist->hash-table
    `(("notifyResult" . ,notify-result)
      ("notifyBinaryResult" . ,notify-binary-result)
      ("notifyEvent" . ,notify-client-event)
      ("startApplication" . ,start-application))
    'equal?))

(define *binary-data-table-atom* (atom (make-hash-table 'equal?)))

(define (start-websocket-dispatcher! sock in out)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (atomic-update! *num-dispatcher* (^x (+ x 1)))
          (let* ((sel (make <selector>))
                 (ctx (make <websocket-server-context>))
                 (next-id (make-id-generator #xffffffff))
                 (app-context (make-application-context 1 2 out)))
            (define (close-websocket status data)
              (log-info "WebSocket closed: code=~a, data=(~a)" status data)
              (selector-delete! sel in #f #f)
              (close-input-port in))
            (define (receive-json json-str)
              (let* ((params (vector->list (parse-json-string json-str)))
                     (cmd (car params))
                     (args (cdr params))
                     (proc (hash-table-get json-command-table cmd #f)))
                (cond
                  (proc
                   (apply proc args))
                  (else
                   (log-error "Invalid data received: ~s" params)))))
            (define (receive-binary data)
              (let1 id (get-u32 data 0)
                (atomic *binary-data-table-atom*
                  (lambda (tbl)
                    (hash-table-put! tbl id (uvector-alias <u8vector> data 4))))))

            (parameterize ((json-special-handler (lambda (sym)
                                                   (case sym
                                                     ((false) #f)
                                                     ((true) #t)
                                                     (else sym))))
                           (application-context app-context))
              (selector-add! sel in (lambda (in flag)
                                      (while (and (byte-ready? in) (not (port-closed? in)))
                                        (dispatch-payload ctx
                                                          in
                                                          out
                                                          :text-handler receive-json
                                                          :binary-handler receive-binary
                                                          :close-handler close-websocket)))
                             '(r))

              (let1 now (time->seconds (current-time))
                (let loop ((now now)
                           (next-update-sec (+ now (refresh-cycle-second))))
                  (set-next-update-time! (make-time-from-second time-utc next-update-sec))
                  (cond
                    ((port-closed? in)
                     #f)
                    ((<= next-update-sec now)
                     (flush-commands)
                     (invoke-event-handler 'update)
                     (let1 now (time->seconds (current-time))
                       (loop now (+ now (refresh-cycle-second)))))
                    (else
                     (selector-select sel (* (- next-update-sec now) 1000))
                     (loop (time->seconds (current-time)) next-update-sec)))))

              (for-each (lambda (pool)
                          (terminate-all! pool :cancel-queued-jobs #t))
                        (list (main-thread-pool) (worker-thread-pool)))
              (log-debug "WebSocket dispatcher finished")
              (close-input-port in)
              (close-output-port out)
              (connection-close sock)
              (connection-shutdown sock 'both)
              (atomic-update! *num-dispatcher* (lambda (x)
                                                 (let1 num (- x 1)
                                                   (when (= num 0)
                                                     (terminate-server-loop *control-channel* 0))
                                                   num))))))))))

(define-http-handler "/s"
  (lambda (req app)
    (let ((in (request-iport req))
          (out (request-oport req)))
      (let-params req ((upgrade "h")
                       (connection "h")
                       (sec-websocket-key "h")
                       (sec-websocket-version "h"))
                  (cond
                    ((and (string-contains-ci upgrade "websocket")
                          (string-contains-ci connection "upgrade")
                          sec-websocket-key
                          (equal? sec-websocket-version "13"))
                     (format out
                             "HTTP/~a 101 Switching Protocols\r\n"
                             (ref req 'http-version))
                     (format out "Host: ~a\r\n" (ref req 'server-host))
                     (format out "Upgrade: websocket\r\n")
                     (format out "Connection: Upgrade\r\n")
                     (format out
                             "Sec-WebSocket-Accept: ~a\r\n"
                             (base64-encode-string
                               (sha1-digest-string
                                 (string-append sec-websocket-key
                                                "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
                     (format out "\r\n")
                     (flush out)
                     (log-debug "WebSocket connected")
                     (start-websocket-dispatcher! (request-socket req) in out)
                     req)
                    (else
                     (respond/ng req 400)))))))

(define *resource-table-atom* (atom (make-hash-table 'equal?)))

(define (register-resource! content-type data)
  (atomic *resource-table-atom*
    (lambda (tbl)
      (let loop ()
        (let1 id (number->string (random-integer #xffffffff) 36)
          (cond
            ((hash-table-contains? tbl id)
             (loop))
            (else
             (hash-table-put! tbl id (cons content-type data))
             (format "/r?id=~a" id))))))))

(define-method resource-url ((filename <string>) :key (content-type #f))
  (let1 data (call-with-input-file filename port->uvector)
    (register-resource! (or content-type
                            (estimate-content-type filename))
                        data)))

(define-http-handler "/r"
  (lambda (req app)
    (let-params req ((id "q"))
                (atomic *resource-table-atom*
                  (lambda (tbl)
                    (match-let1 (content-type . data) (hash-table-get tbl id (cons #f #f))
                      (cond
                        ((and content-type data)
                         (hash-table-delete! tbl id)
                         (respond/ok req data :content-type content-type))
                        (else
                         (respond/ng req 404)))))))))

;;;

(define (call-text-command command-name :rest args)
  (with-send-context
    (lambda (ctx)
      (send-text-frame (slot-ref ctx 'websocket-output-port)
                       (construct-json-string (apply vector command-name args))))))

(define binary-command-table
  (let1 tbl (make-hash-table 'eq?)
    (map-with-index (lambda (i cmd)
                      (hash-table-put! tbl cmd i))
                    '(
                      app-close
                      unlink-proxy-object
                      register-binary

                      listen-window-event
                      listen-canvas-event
                      ))
    tbl))

(define (write-variant n out)
  (cond
    ((zero? n)
     (write-u8 0 out))
    (else
     (let loop ((n n))
       (cond
         ((zero? n)
          #t)
         ((<= n #x7f)
          (write-u8 n out))
         (else
          (write-u8 (logior (logand n #x7f) #x80) out)
          (loop (ash n -7))))))))

(define (call-command cmd types args)
  (with-send-context
    (lambda (ctx)
      (let1 out (slot-ref ctx 'buffer-output-port)
        (write-u16 (if (symbol? cmd)
                       (hash-table-get binary-command-table cmd)
                       cmd)
                   out
                   'little-endian)
        (map (lambda (type arg)
               (match type
                 ('uint
                  (write-variant arg out))
                 ('int
                  (write-variant (if (<= 0 arg)
                                     (ash arg 1)
                                     (- (ash (- n) 1) 1))
                                 out))
                 ('f32
                  (write-f32 arg out 'little-endian))
                 ('f64
                  (write-f64 arg out 'little-endian))
                 ('s16
                  (write-s16 arg out 'little-endian))
                 ('s32
                  (write-s32 arg out 'little-endian))
                 ('s8
                  (write-s8 arg out 'little-endian))
                 ('u16
                  (write-u16 arg out 'little-endian))
                 ('u32
                  (write-u32 arg out 'little-endian))
                 ('u8
                  (write-u8 arg out 'little-endian))
                 ('u8vector
                  (write-u32 (u8vector-length arg) out 'little-endian)
                  (write-uvector arg out))
                 ('f64vector
                  (write-u32 (f64vector-length arg) out 'little-endian)
                  (write-uvector arg out 0 -1 'little-endian))
                 ('boolean
                   (write-u8 (if arg 1 0) out 'little-endian))
                 ('json
                  (let1 id (json-next-id)
                    (push! (slot-ref ctx 'json-pair-buffer) (vector id arg))
                    (write-u32 id out 'little-endian)))
                 ('future
                  (let1 id (register-future! arg)
                    (write-u32 id out 'little-endian)))
                 ('proxy
                  (write-u32 (proxy-object-id arg) out 'little-endian))
                 (('resource content-type)
                  (let* ((url (register-resource! content-type arg))
                         (id (json-next-id)))
                    (push! (slot-ref ctx 'json-pair-buffer) (vector id url))
                    (write-u32 id out 'little-endian)))
                 ((? symbol? enum-name)
                  (unless (hash-table-contains? *enum-table* enum-name)
                    (errorf "Invalid type: ~a" type))
                  (write-u8 (enum-value enum-name arg) out 'little-endian))))
             types args)))))

(define (flush-commands)
  (with-send-context
    (lambda (ctx)
      (let ((json-pair-buffer (slot-ref ctx 'json-pair-buffer))
            (output-data (get-output-uvector (slot-ref ctx 'buffer-output-port))))
        (unless (null? json-pair-buffer)
          (call-text-command "registerArgs" (list->vector json-pair-buffer)))
        (unless (= (uvector-length output-data) 0)
          (send-binary-frame (slot-ref ctx 'websocket-output-port) output-data)))
      (slot-set! ctx 'json-pair-buffer '())
      (slot-set! ctx 'buffer-output-port (open-output-uvector)))))

;;;

(define (parse-arg-spec spec)
  (match spec
    ((? symbol? spec)
     (parse-arg-spec (map string->symbol (string-split (symbol->string spec) "::"))))
    ((var type)
     (list var type))
    ((var)
     (list var 'json))))

(define binary-command-next-id (make-id-generator #x7fffffff))

(define *enum-table* (make-hash-table))

(define-class <js-procedure> ()
  ((types :init-keyword :types)
   (call-id :init-keyword :call-id)
   (run-id :init-keyword :run-id)))

(define (compile-jslambda jsmodule-name arg-specs body)
  (let* ((var-type-list (map parse-arg-spec arg-specs))
         (base-id (binary-command-next-id))
         (run-id (ash base-id 1))
         (call-id (+ (ash base-id 1) 1))
         (name (gensym))
         (ds (gensym)))
    (register-js-stmt! jsmodule-name
                       `(begin
                          (define (,name %future-id ,ds)
                            (let ,(map (match-lambda
                                         ((var type)
                                          `(,var ,(match type
                                                    ('f32
                                                     `((ref ,ds getFloat32)))
                                                    ('f64
                                                     `((ref ,ds getFloat64)))
                                                    ('s16
                                                     `((ref ,ds getInt16)))
                                                    ('s32
                                                     `((ref ,ds getInt32)))
                                                    ('s8
                                                     `((ref ,ds getInt8)))
                                                    ('u16
                                                     `((ref ,ds getUint16)))
                                                    ('u32
                                                     `((ref ,ds getUint32)))
                                                    ('u8
                                                     `((ref ,ds getUint8)))
                                                    ('u8vector
                                                     `((ref ,ds getUint8Array)))
                                                    ('f64vector
                                                     `((ref ,ds getFloat64Array)))
                                                    ('boolean
                                                      `((ref ,ds getBoolean)))
                                                    ('json
                                                     `((ref ,ds getJson)))
                                                    ('proxy
                                                     `((ref ,ds getProxyObject)))
                                                    ((? symbol? enum-name)
                                                     (unless (hash-table-contains? *enum-table* enum-name)
                                                       (errorf "Invalid type: ~a" type))
                                                     `((ref ,ds getEnum) ,(symbol->string enum-name)))
                                                    ))))
                                       var-type-list)
                              ,@body)
                            undefined)
                          (vector-set! binaryCommands ,call-id ,name)
                          (vector-set! binaryCommands ,run-id ,name)))
    (make <js-procedure> :call-id call-id :run-id run-id :types (map (cut list-ref <> 1) var-type-list))))

(define-jsise-macro result
  ((vals ...)
   `(notifyValues %future-id ,(list->vector vals))))

(define-jsise-macro result-u8array
  ((data)
   `(notifyBinaryData %future-id ,data)))

(define-jsise-macro result-error
  ((err)
   `(notifyException %future-id ((ref ,err toString)))))

(define-macro (jslambda arg-specs :rest body)
  (compile-jslambda (module-name->path (module-name (current-module))) arg-specs body))

(define (jscall js-proc provider :rest args)
  (let1 future (make <graviton-future>)
    (when provider
      (slot-set! future 'result-maker provider))
    (call-command (slot-ref js-proc 'call-id)
                  (cons 'future (slot-ref js-proc 'types))
                  (cons future args))
    future))

(define (jsrun js-proc :rest args)
  (call-command (slot-ref js-proc 'run-id)
                (slot-ref js-proc 'types)
                args))

(define-macro (jslet var-spec :rest body)
  `(jsrun (jslambda ,var-spec
            ,@body)
          ,@(map (lambda (spec)
                   (list-ref (parse-arg-spec spec) 0))
                 var-spec)))

(define-macro (jslet/result var-spec :rest body)
  `(jscall (jslambda ,var-spec
             ,@body)
           #f
           ,@(map (lambda (spec)
                    (list-ref (parse-arg-spec spec) 0))
                  var-spec)))

(define-macro (jslet/result:then provider var-spec :rest body)
  `(jscall (jslambda ,var-spec
             ,@body)
           ,provider
           ,@(map (lambda (spec)
                    (list-ref (parse-arg-spec spec) 0))
                  var-spec)))

(define-class <js-enum> ()
  ((symbol->value-table :init-form (make-hash-table))
   (jsvalue->symbol-table :init-form (make-hash-table 'equal?))))

(define (make-enum jsmodule-name enum-name symbol->jsvalue-list)
  (let ((enum (make <js-enum>))
        (jsvals '()))
    (for-each-with-index (lambda (i symbol->jsvalue)
                           (match symbol->jsvalue
                             ((sym jsval)
                              (hash-table-put! (slot-ref enum 'symbol->value-table) sym i)
                              (hash-table-put! (slot-ref enum 'jsvalue->symbol-table) jsval sym)
                              (push! jsvals jsval))))
                         symbol->jsvalue-list)
    (register-js-stmt! jsmodule-name
                       `(set! (aref enumTable ,(symbol->string enum-name)) ,(list->vector (reverse jsvals))))
    enum))

(define-syntax define-jsenum
  (syntax-rules ()
    ((_ name symbol->jsvalue ...)
     (let1 enum (make-enum (module-name->path (module-name (current-module))) 'name '(symbol->jsvalue ...))
       (hash-table-put! *enum-table* 'name enum)))))

(define (enum-value name sym)
  (let1 enum (hash-table-get *enum-table* name #f)
    (unless enum
      (errorf "enum ~a not found" enum))
    (or (hash-table-get (slot-ref enum 'symbol->value-table) sym #f)
        (errorf "enum symbol ~a not found" sym))))

(define (enum-symbol name jsval)
  (let1 enum (hash-table-get *enum-table* name #f)
    (unless enum
      (errorf "enum ~a not found" enum))
    (or (hash-table-get (slot-ref enum 'jsvalue->symbol-table) jsval #f)
        (errorf "enum jsvalue ~a not found" jsval))))

;;;

(inline-js
  (set! window.onload (lambda ()
                        (for-each (lambda (func)
                                    (func))
                                  initializeFunctions)))

  (set! window.onresize (lambda (e)
                          (let1 elements (ref (document.getElementById "_on") children)
                            (dotimes (i elements.length)
                              (when (equal? (ref (aref elements i) tagName) "CANVAS")
                                (centralizeCanvas (aref elements i)))))))
  )

;;;

(define (window-size)
  (jslet/result ()
    (result window.innerWidth window.innerHeight)))

(define (app-close)
  (jslet ()
    (when webSocket
      (webSocket.close 1000))))

;;;

(define current-canvas (make-parameter #f))

(define-class <canvas> (<proxy-object>)
  ((width :init-keyword :width)
   (height :init-keyword :height)
   (z :init-keyword :z)
   (visible? :init-keyword :visible?)
   (context2d-list :init-form (list (make <context2d>)))
   (context2d :allocation :virtual
              :slot-ref (lambda (canvas)
                          (car (slot-ref canvas 'context2d-list))))))

(define-class <context2d> ()
  ((fill-style :init-value "#000000")
   (font :init-value "10px sans-serif")
   (global-alpha :init-value 1.0)
   (global-composite-operation :init-value 'source-over)
   (image-smoothing-enabled :init-value #t)
   (line-cap :init-value 'butt)
   (line-dash :init-value #())
   (line-dash-offset :init-value 0)
   (line-join :init-value 'miter)
   (line-width :init-value 1.0)
   (miter-limit :init-value 10.0)
   (shadow-blur :init-value 0)
   (shadow-color :init-value "rgba(0, 0, 0, 0)")
   (shadow-offset-x :init-value 0)
   (shadow-offset-y :init-value 0)
   (stroke-style :init-value "#000000")
   (text-align :init-value 'start)
   (text-baseline :init-value 'alphabetic)))

(define (copy-context2d ctx)
  (let1 ctx2 (make <context2d>)
    (for-each (lambda (slot)
                (let1 slot-name (car slot)
                  (slot-set! ctx2 slot-name (slot-ref ctx slot-name))))
              (class-direct-slots <context2d>))
    ctx2))

(define-class <graviton-image> (<proxy-object>)
  ((width :init-keyword :width)
   (height :init-keyword :height)))

(define-class <linear-gradient> ()
  ((x0 :init-keyword :x0)
   (y0 :init-keyword :y0)
   (x1 :init-keyword :x1)
   (y1 :init-keyword :y1)
   (color-stops :init-keyword :color-stops)))

(define (linear-gradient x0 y0 x1 y1 color-stops)
  (make <linear-gradient> :x0 x0 :y0 y0 :x1 x1 :y1 y1 :color-stops color-stops))

(define-method style->json ((linear-gradient <linear-gradient>))
  `(("type" . "linear-gradient")
    ("x0" . ,(slot-ref linear-gradient 'x0))
    ("y0" . ,(slot-ref linear-gradient 'y0))
    ("x1" . ,(slot-ref linear-gradient 'x1))
    ("y1" . ,(slot-ref linear-gradient 'y1))
    ("color-stops" . ,(list->vector (map list->vector (slot-ref linear-gradient 'color-stops))))))

(define-class <radial-gradient> ()
  ((x0 :init-keyword :x0)
   (y0 :init-keyword :y0)
   (r0 :init-keyword :r0)
   (x1 :init-keyword :x1)
   (y1 :init-keyword :y1)
   (r1 :init-keyword :r1)
   (color-stops :init-keyword :color-stops)))

(define (radial-gradient x0 y0 r0 x1 y1 r1 color-stops)
  (make <radial-gradient> :x0 x0 :y0 y0 :r0 r0 :x1 x1 :y1 y1 :r1 r1 :color-stops color-stops))

(define-method style->json ((radial-gradient <radial-gradient>))
  `(("type" . "radial-gradient")
    ("x0" . ,(slot-ref radial-gradient 'x0))
    ("y0" . ,(slot-ref radial-gradient 'y0))
    ("r0" . ,(slot-ref radial-gradient 'r0))
    ("x1" . ,(slot-ref radial-gradient 'x1))
    ("y1" . ,(slot-ref radial-gradient 'y1))
    ("r1" . ,(slot-ref radial-gradient 'r1))
    ("color-stops" . ,(list->vector (map list->vector (slot-ref radial-gradient 'color-stops))))))

(define-method style->json ((color <string>))
  color)

(define-class <pattern> ()
  ((canvas :init-keyword :canvas
           :init-value #f)
   (image :init-keyword :image
          :init-value #f)
   (repetition :init-keyword :repetition)))

(define-constant repetition-alist
  '((repeat . "repeat")
    (repeat-x . "repeat-x")
    (repeat-y . "repeat-y")
    (no-repeat . "no-repeat")))

(define (repetition-name repetition)
  (or (assoc-ref repetition-alist repetition #f)
      (errorf "Invalid repetition: ~s" repetition)))

(define-method pattern ((canvas <canvas>) :optional (repetition 'repeat))
  (make <pattern> :canvas canvas :repetition (repetition-name repetition)))

(define-method pattern ((image <graviton-image>) :optional (repetition 'repeat))
  (make <pattern> :image image :repetition (repetition-name repetition)))

(define-method style->json ((pattern <pattern>))
  `(("type" . "pattern")
    ("canvas" . ,(and-let1 canvas (slot-ref pattern 'canvas)
                   (proxy-object-id canvas)))
    ("image" . ,(and-let1 image (slot-ref pattern 'image)
                  (proxy-object-id image)))
    ("repetition" . ,(slot-ref pattern 'repetition))))


(define-jsfn (%make-canvas canvas-id width height z visible?)
  (let ((canvas (document.createElement "canvas")))
    (set! canvas.id (+ "canvas" canvas-id))
    (set! canvas.width width)
    (set! canvas.height height)
    (linkProxyObject canvas-id canvas)

    (centralizeCanvas canvas)
    (set! canvas.style.zIndex z)
    ((ref (document.getElementById "_on") appendChild) canvas)

    (when window.isElectron
      (window.showBrowserWindow))

    (if visible?
        (set! canvas.style.visibility "visible")
        (set! canvas.style.visibility "hidden"))
    canvas))

(define-jsvar *working-images* (make Set))

(define (make-canvas width height :key (z 0) (visible? #t))
  (let* ((canvas (make <canvas> :width width :height height :z z :visible? visible?))
         (canvas-id (slot-ref canvas 'id)))
    (jslet (canvas-id::u32
            width::u32
            height::u32
            z::u32
            visible?::boolean)
      (%make-canvas canvas-id width height z visible?))
    (when visible?
      (current-canvas canvas))
    canvas))

(define (load-canvas filename :key (z 0) (visible? #t) (content-type #f))
  (let* ((canvas (make <canvas> :z z :visible? visible?))
         (canvas-id (slot-ref canvas 'id))
         (url (resource-url filename :content-type content-type)))
    (jslet/result:then (lambda (w h)
                         (slot-set! canvas 'width w)
                         (slot-set! canvas 'height h)
                         canvas)
        (canvas-id::u32
         url::json
         z::u32
         visible?::boolean)
      (let ((img (make Image)))
        (*working-images*.add img)
        (set! img.src url)
        (set! img.onload (lambda ()
                           (let* ((canvas (%make-canvas canvas-id img.width img.height z visible?))
                                  (ctx (canvas.getContext "2d")))
                             (ctx.drawImage img 0 0)
                             (*working-images*.delete img)
                             (result canvas.width canvas.height))))
        (set! img.onerror (lambda ()
                            (*working-images*.delete img)
                            (result-error "Load image failed.")))))))

(define (set-canvas-visible! canvas visible?)
  (jslet (canvas::proxy
          visible?::boolean)
    (cond
      (visible?
       (set! canvas.style.visibility "visible")
       (when window.isElectron
         (window.showBrowserWindow)))
      (else
       (set! canvas.style.visibility "hidden"))))
  (slot-set! canvas 'visible? visible?))

(define (current-fill-style)
  (~ (current-canvas) 'context2d 'fill-style))

(define (set-fill-style! style)
  (set! (~ (current-canvas) 'context2d 'fill-style) style)
  (let ((canvas (current-canvas))
        (style-json (style->json style)))
    (jslet (canvas::proxy
            style-json::json)
      (let ((ctx (canvas.getContext "2d")))
        (set! ctx.fillStyle (obj2style ctx style-json))))))

(define (current-font)
  (~ (current-canvas) 'context2d 'font))

(define (set-font! font)
  (set! (~ (current-canvas) 'context2d 'font) font)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            font::json)
      (set! (ref (canvas.getContext "2d") font) font))))

(define (current-global-alpha)
  (~ (current-canvas) 'context2d 'global-alpha))

(define (set-global-alpha! alpha)
  (set! (~ (current-canvas) 'context2d 'global-alpha) alpha)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            alpha::f64)
      (set! (ref (canvas.getContext "2d") globalAlpha) alpha))))

(define (current-global-composite-operation)
  (~ (current-canvas) 'context2d 'global-composite-operation))

(define-jsenum global-composite-operation-enum
  (source-over "source-over")
  (source-in "source-in")
  (source-out "source-out")
  (source-atop "source-atop")
  (destination-over "destination-over")
  (destination-in "destination-in")
  (destination-out "destination-out")
  (destination-atop "destination-atop")
  (lighter "lighter")
  (copy "copy")
  (xor "xor")
  (multiply "multiply")
  (screen "screen")
  (overlay "overlay")
  (darken "darken")
  (lighten "lighten")
  (color-dodge "color-dodge")
  (color-burn "color-burn")
  (hard-light "hard-light")
  (soft-light "soft-light")
  (difference "difference")
  (exclusion "exclusion")
  (hue "hue")
  (saturation "saturation")
  (color "color")
  (luminosity "luminosity"))

(define (set-global-composite-operation! op)
  (set! (~ (current-canvas) 'context2d 'global-composite-opration) op)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            op::global-composite-operation-enum)
      (set! (ref (canvas.getContext "2d") globalCompositeOperation) op))))

(define (current-image-smoothing-enabled?)
  (~ (current-canvas) 'context2d 'image-smoothing-enabled))

(define (set-image-smoothing-enabled! flag)
  (set! (~ (current-canvas) 'context2d 'image-smoothing-enabled) flag)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            flag::boolean)
      (set! (ref (canvas.getContext "2d") imageSmoothingEnabled) flag))))

(define (current-line-cap)
  (~ (current-canvas) 'context2d 'line-cap))

(define-jsenum line-cap-enum
  (butt "butt")
  (round "round")
  (square "square"))

(define (set-line-cap! opt)
  (set! (~ (current-canvas) 'context2d 'line-cap) opt)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            opt::line-cap-enum)
      (set! (ref (canvas.getContext "2d") lineCap) opt))))

(define (current-line-dash)
  (~ (current-canvas) 'context2d 'line-dash))

(define (set-line-dash! segments)
  (set! (~ (current-canvas) 'context2d 'line-dash) segments)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            segments::json)
      ((ref (canvas.getContext "2d") setLineDash) segments))))

(define (current-line-dash-offset)
  (~ (current-canvas) 'context2d 'line-dash-offset))

(define (set-line-dash-offset! offset)
  (set! (~ (current-canvas) 'context2d 'line-dash-offset) offset)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            offset::f64)
      (set! (ref (canvas.getContext "2d") lineDashOffset) offset))))

(define (current-line-join)
  (~ (current-canvas) 'context2d 'line-join))

(define-jsenum line-join-enum
  (bevel "bevel")
  (round "round")
  (miter "miter"))

(define (set-line-join! opt)
  (set! (~ (current-canvas) 'context2d 'line-join) opt)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            opt::line-join-enum)
      (set! (ref (canvas.getContext "2d") lineJoin) opt))))

(define (current-line-width)
  (~ (current-canvas) 'context2d 'line-width))

(define (set-line-width! w)
  (set! (~ (current-canvas) 'context2d 'line-width) w)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            w::f64)
      (set! (ref (canvas.getContext "2d") lineWidth) w))))

(define (current-miter-limit)
  (~ (current-canvas) 'context2d 'miter-limit))

(define (set-miter-limit! limit)
  (set! (~ (current-canvas) 'context2d 'miter-limit) limit)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            limit::f64)
      (set! (ref (canvas.getContext "2d") miterLimit) limit))))

(define (current-shadow-blur)
  (~ (current-canvas) 'context2d 'shadow-blur))

(define (set-shadow-blur! level)
  (set! (~ (current-canvas) 'context2d 'shadow-blur) level)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            level::f64)
      (set! (ref (canvas.getContext "2d") shadowBlur) level))))

(define (current-shadow-color)
  (~ (current-canvas) 'context2d 'shadow-color))

(define (set-shadow-color! color)
  (set! (~ (current-canvas) 'context2d 'shadow-color) color)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            color::json)
      (set! (ref (canvas.getContext "2d") shadowColor) color))))

(define (current-shadow-offset-x)
  (~ (current-canvas) 'context2d 'shadow-offset-x))

(define (set-shadow-offset-x! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-x) offset)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            offset::f64)
      (set! (ref (canvas.getContext "2d") shadowOffsetX) offset))))

(define (current-shadow-offset-y)
  (~ (current-canvas) 'context2d 'shadow-offset-y))

(define (set-shadow-offset-y! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-y) offset)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            offset::f64)
      (set! (ref (canvas.getContext "2d") shadowOffsetY) offset))))

(define (current-stroke-style)
  (~ (current-canvas) 'context2d 'stroke-style))

(define (set-stroke-style! style)
  (set! (~ (current-canvas) 'context2d 'stroke-style) style)
  (let ((canvas (current-canvas))
        (style-json (style->json style)))
    (jslet (canvas::proxy
            style-json::json)
      (let ((ctx (canvas.getContext "2d")))
        (set! ctx.strokeStyle (obj2style ctx style-json))))))

(define (current-text-align)
  (~ (current-canvas) 'context2d 'text-align))

(define-jsenum text-align-enum
  (left "left")
  (right "right")
  (center "center")
  (start "start")
  (end "end"))

(define (set-text-align! align)
  (set! (~ (current-canvas) 'context2d 'text-align) align)
  (let ((canvas (current-canvas)))
    (set! (ref (canvas.getContext "2d") textAlign) align)))

(define (current-text-baseline)
  (~ (current-canvas) 'context2d 'text-baseline))

(define-jsenum text-baseline-enum
  (top "top")
  (hanging "hanging")
  (middle "middle")
  (alphabetic "alphabetic")
  (ideographic "ideographic")
  (bottom "bottom"))

(define (set-text-baseline! opt)
  (set! (~ (current-canvas) 'context2d 'text-baseline) opt)
  (let ((canvas (current-canvas)))
    (set! (ref (canvas.getContext "2d") textBaseline) opt)))

(define (arc x y radius start-angle end-angle :optional (anti-clockwise #f))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            radius::s32
            start-angle::f64
            end-angle::f64
            anti-clockwise::boolean)
      ((ref (canvas.getContext "2d") arc) x y radius start-angle end-angle anti-clockwise))))

(define (arc-to x1 y1 x2 y2 radius)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x1::s32
            y1::s32
            x2::s32
            y2::s32
            radius::s32)
      ((ref (canvas.getContext "2d") arcTo) x1 y1 x2 y2 radius))))

(define (begin-path)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy)
      ((ref (canvas.getContext "2d") beginPath)))))

(define (bezier-curve-to cp1x cp1y cp2x cp2y x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            cp1x::s32
            cp1y::s32
            cp2x::s32
            cp2y::s32
            x::s32
            y::s32)
      ((ref (canvas.getContext "2d") bezierCurveTo) cp1x cp1y cp2x cp2y x y))))

(define (clear-rect x y w h)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            w::s32
            h::s32)
      ((ref (canvas.getContext "2d") clearRect) x y w h))))

(define-jsenum fillrule-enum
  (nonzero "nonzero")
  (evenodd "evenodd"))

(define (clip :optional (rule 'nonzero))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            rule::fillrule-enum)
      ((ref (canvas.getContext "2d") clip) rule))))

(define (close-path)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy)
      ((ref (canvas.getContext "2d") closePath)))))

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
      (else
       #f))))

(define-method draw-canvas ((src-canvas <canvas>) dx dy)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            src-canvas::proxy
            dx::s32
            dy::s32)
      ((ref (canvas.getContext "2d") drawImage) src-canvas dx dy))))

(define-method draw-canvas ((src-canvas <canvas>) dx dy dw dh)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            src-canvas::proxy
            dx::s32
            dy::s32
            dw::s32
            dh::s32)
      ((ref (canvas.getContext "2d") drawImage) src-canvas dx dy dw dh))))

(define-method draw-canvas ((src-canvas <canvas>) sx sy sw sh dx dy dw dh)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            src-canvas::proxy
            sx::s32
            sy::s32
            sw::s32
            sh::s32
            dx::s32
            dy::s32
            dw::s32
            dh::s32)
      ((ref (canvas.getContext "2d") drawImage) src-canvas sx sy sw sh dx dy dw dh))))

(define (ellipse x y radius-x radius-y rotation start-angle end-angle :optional (anti-clockwise? #f))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            radius-x::s32
            radius-y::s32
            rotation::f64
            start-angle::f64
            end-angle::f64
            anti-clockwise?::boolean)
      ((ref (canvas.getContext "2d") ellipse)
       x y radius-x radius-y rotation start-angle end-angle anti-clockwise?))))

(define (fill :optional (rule 'nonzero))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            rule::fillrule-enum)
      ((ref (canvas.getContext "2d") fill) rule))))

(define (fill-rect x y w h)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            w::s32
            h::s32)
      ((ref (canvas.getContext "2d") fillRect) x y w h))))

(define (fill-text text x y :optional (max-width 0))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            text::json
            x::s32
            y::s32
            max-width::s32)
      (if (< 0 max-width)
          ((ref (canvas.getContext "2d") fillText) text x y max-width)
          ((ref (canvas.getContext "2d") fillText) text x y)))))

(define (get-image-data sx sy sw sh)
  (let* ((canvas (current-canvas))
         (image (make <graviton-image> :width sw :height sh))
         (image-id (slot-ref image 'id)))
    (jslet (canvas::proxy
            image-id::u32
            sx::s32
            sy::s32
            sw::s32
            sh::s32)
      (let ((image ((ref (canvas.getContext "2d") getImageData) sx sy sw sh)))
        (linkProxyObject image-id image)))
    image))

(define (is-point-in-path? x y :optional (rule 'nonzero))
  (let ((canvas (current-canvas)))
    (jslet/result (canvas::proxy
                   x::s32
                   y::s32
                   rule::fillrule-enum)
      (result ((ref (canvas.getContext "2d") isPointInPath) x y rule)))))

(define (is-point-in-stroke? x y)
  (let ((canvas (current-canvas)))
    (jslet/result (canvas::proxy
                   x::s32
                   y::s32)
      (result ((ref (canvas.getContext "2d") isPointInStroke) x y)))))

(define (line-to x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32)
      ((ref (canvas.getContext "2d") lineTo) x y))))

(define-class <text-metrics> ()
  ((width :init-keyword :width)))

(define (measure-text text)
  (let ((canvas (current-canvas)))
    (jslet/result:then (lambda (width)
                         (make <text-metrics> :width width))
        (canvas::proxy
         text::json)
      (let ((text-metrics ((ref (canvas.getContext "2d") measureText) text)))
        (result text-metrics.width)))))

(define (move-to x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32)
      ((ref (canvas.getContext "2d") moveTo) x y))))

(define-method put-image-data ((image <graviton-image>) dx dy)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            image::proxy
            dx::s32
            dy::s32)
      ((ref (canvas.getContext "2d") putImageData) image dx dy))))

(define-method put-image-data ((image <graviton-image>) dx dy dirty-x dirty-y dirty-width dirty-height)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            image::proxy
            dx::s32
            dy::s32
            dirty-x::s32
            dirty-y::s32
            dirty-width::s32
            dirty-height::s32)
      ((ref (canvas.getContext "2d") putImageData) image dx dy dirty-x dirty-y dirty-width dirty-height))))

(define (quadratic-curve-to cpx cpy x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            cpx::s32
            cpy::s32
            x::s32
            y::s32)
      ((ref (canvas.getContext "2d") quadraticCurveTo) cpx cpy x y))))

(define (rect x y w h)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            w::s32
            h::s32)
      ((ref (canvas.getContext "2d") rect) x y w h))))

(define (restore-context)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy)
      ((ref (canvas.getContext "2d") restore))))
  (pop! (slot-ref (current-canvas) 'context2d-list)))

(define (rotate angle)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            angle::f64)
      ((ref (canvas.getContext "2d") rotate) angle))))

(define (save-context)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy)
      ((ref (canvas.getContext "2d") save))))
  (let1 ctx2 (copy-context2d (slot-ref (current-canvas) 'context2d))
    (push! (slot-ref (current-canvas) 'context2d-list) ctx2)))

(define (scale x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::f64
            y::f64)
      ((ref (canvas.getContext "2d") scale) x y))))

(define (set-transform! a b c d e f)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            a::f64
            b::f64
            c::f64
            d::f64
            e::f64
            f::f64)
      ((ref (canvas.getContext "2d") setTransform) a b c d e f))))

(define (stroke)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy)
      ((ref (canvas.getContext "2d") stroke)))))

(define (stroke-rect x y w h)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32
            w::s32
            h::s32)
      ((ref (canvas.getContext "2d") strokeRect) x y w h))))

(define (stroke-text text x y :optional (max-width 0))
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            text::json
            x::s32
            y::s32
            max-width::s32)
      (if (< 0 max-width)
          ((ref (canvas.getContext "2d") strokeText) text x y max-width)
          ((ref (canvas.getContext "2d") strokeText) text x y)))))

(define (transform a b c d e f)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            a::f64
            b::f64
            c::f64
            d::f64
            e::f64
            f::f64)
      ((ref (canvas.getContext "2d") transform) a b c d e f))))

(define (translate x y)
  (let ((canvas (current-canvas)))
    (jslet (canvas::proxy
            x::s32
            y::s32)
      ((ref (canvas.getContext "2d") translate) x y))))

(define (create-image-data w h)
  (let* ((canvas (current-canvas))
         (image (make <graviton-image> :width w :height h))
         (image-id (slot-ref image 'id)))
    (jslet (canvas::proxy
            image-id::u32
            w::s32
            h::s32)
      (let ((image ((ref (canvas.getContext "2d") createImageData) w h)))
        (linkProxyObject image-id image)))
    image))

(define (upload-image-data image data)
  (jslet (image::proxy
          data::u8vector)
    (image.data.set data)))

(define (download-image-data image)
  (jslet/result (image::proxy)
    (result-u8array image.data)))

;; (define-jsvar *listen-state-table* (object))

(define (set-window-event-handler! event-type proc)
  (let ((event-name (symbol->string event-type))
        (enable? (if proc #t #f)))
    (jslet (event-name::json
            enable?::boolean)
      (set! (aref listenStateTable event-name) enable?)))
  (add-event-listener! event-type proc))

(define (canvas-event-type canvas event-type)
  (string->symbol (format "_canvas_~a_~a" (slot-ref canvas 'id) event-type)))

(define (set-canvas-event-handler! canvas event-type proc)
  (let ((event-id (symbol->string (canvas-event-type canvas event-type)))
        (event-name (symbol->string event-type))
        (enable? (if proc #t #f)))
    (jslet (canvas::proxy
            event-id::json
            event-name::json
            enable?::boolean)
      (let ((handler (if enable?
                         (lambda (e)
                           (notifyEvent event-id (createMouseEvent canvas e)))
                         null)))
        (case event-name
          (("click")
           (set! canvas.onclick handler))
          (("dblclick")
           (set! canvas.ondblclick handler))
          (("contextmenu")
           (set! canvas.oncontextmenu handler))
          (("mousedown")
           (set! canvas.onmousedown handler))
          (("mouseup")
           (set! canvas.onmouseup handler))
          (("mouseover")
           (set! canvas.onmouseover handler))
          (("mouseout")
           (set! canvas.onmouseout handler))
          (("mousemove")
           (set! canvas.onmousemove handler))
          (("wheel")
           (set! canvas.onwheel handler))
          (else
           (throw (+ "Unsupported event: " event-name))))))
    (add-event-listener! (string->symbol event-id) proc)))

;;;

(define (loop-frame proc)
  (let ((exit? #f))
    (define (break)
      (set! exit? #t))
    (while (not exit?)
      (let1 start-sec (time->seconds (current-time))
        (proc break)
        (let* ((frame-sec (refresh-cycle-second))
               (elapse-sec (- (time->seconds (current-time)) start-sec)))
          (when (< frame-sec elapse-sec)
            (log-debug "Frame dropped. renderer procedure consumes ~a sec. It exceeds one frame sec: ~a"
                       elapse-sec
                       frame-sec))
          (await-window-event 'update))))))

;;;


;;;

(define-jsenum oscillator-type-enum
  (sine "sine")
  (square "square")
  (sawtooth "sawtooth")
  (triangle "triangle"))

(define-class <audio-media-element-node> (<proxy-object>)
  ((duration :init-value #f)))

(define (load-audio filename :key (content-type #f))
  (let* ((node (make <audio-media-element-node>))
         (node-id (slot-ref node 'id))
         (url (resource-url filename :content-type content-type)))
    (jslet/result:then (lambda (duration)
                         (slot-set! node 'duration duration)
                         node)
        (node-id::u32
         url::json)
      (let ((audio (make Audio url)))
        (set! audio.onloadedmetadata (lambda ()
                                       (let ((source-node (audioContext.createMediaElementSource audio)))
                                         (linkProxyObject node-id source-node)
                                         (source-node.connect audioContext.destination)
                                         (result audio.duration))))
        (set! audio.onstalled (lambda ()
                                (result-error "Load audio failed.")))))))

(define (play-audio audio)
  (jslet (audio::proxy)
    (audio.mediaElement.play)))

(define (pause-audio audio)
  (jslet (audio::proxy)
    (audio.mediaElement.pause)))

(define-class <audio-buffer> (<proxy-object>)
  ((sample-rate)
   (length)
   (duration)
   (number-of-channels)))

(define (load-pcm filename :key (content-type #f))
  (let* ((pcm (make <audio-buffer>))
         (object-id (slot-ref pcm 'id))
         (url (resource-url filename :content-type content-type)))
    (jslet/result:then (lambda (sample-rate len duration num-of-channels)
                         (slot-set! pcm 'sample-rate sample-rate)
                         (slot-set! pcm 'length len)
                         (slot-set! pcm 'duration duration)
                         (slot-set! pcm 'number-of-channels num-of-channels)
                         pcm)
        (object-id::u32
         url::json)
      (let ((req (make XMLHttpRequest)))
        (req.open "GET" url #t)
        (set! req.responseType "arraybuffer")
        (set! req.onload (lambda ()
                           (cond
                             ((equal? req.status 200)
                              (let ((buf req.response))
                                ((ref (audioContext.decodeAudioData buf) then)
                                 (lambda (decoded-data)
                                   (linkProxyObject object-id decoded-data)
                                   (result decoded-data.sampleRate
                                           decoded-data.length
                                           decoded-data.duration
                                           decoded-data.numberOfChannels))
                                 (lambda (reason)
                                   (result-error reason)))))
                             (else
                              (result-error (+ "Load PCM failed. (status:" req.status ")"))))))
        (req.send)))))

(define-method play-wave (channel-num type (freq <real>) len :optional (gain 1.0))
  (play-wave channel-num type (f64vector freq) len gain))

(define-method play-wave (channel-num type (freqs <f64vector>) len :optional (gain 1.0))
  (jslet (channel-num::u8
          type::oscillator-type-enum
          freqs::f64vector
          len::f64
          gain::f64)
    (let ((channel (aref audioChannels channel-num))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (dotimes (i freqs.length)
        (let ((oscillator-node (make OscillatorNode audioContext))
              (gain-node (audioContext.createGain)))
          (set! oscillator-node.type type)
          (set! oscillator-node.frequency.value (aref freqs i))
          (set! gain-node.gain.value gain)
          (oscillator-node.connect gain-node)
          (gain-node.connect audioContext.destination)
          (oscillator-node.start start-time)
          (oscillator-node.stop end-time)))
      (set! channel.lastPlaySec end-time))))

(define (play-rest channel-num len)
  (jslet (channel-num::u8
          len::f64)
    (let ((channel (aref audioChannels channel-num))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (set! channel.lastPlaySec end-time))))

(define (play-pcm channel-num pcm-data :optional (detune 0) (playback-rate 1.0) (loop-range #f) (len #f) (gain 1.0))
  (let ((loop? (if loop-range #t #f))
        (loop-start (if loop-range (list-ref loop-range 0) 0))
        (loop-end (if loop-range (list-ref loop-range 1) 0))
        (len (or len
                 (slot-ref pcm-data 'duration))))
  (jslet (channel-num::u8
          pcm-data::proxy
          detune::f64
          playback-rate::f64
          loop?::boolean
          loop-start::f64
          loop-end::f64
          len::f64
          gain::f64)
    (let ((channel (aref audioChannels channel-num))
          (pcm-node (audioContext.createBufferSource))
          (gain-node (audioContext.createGain))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (set! pcm-node.buffer pcm-data)
      (unless (equal? detune 0)
        (set! pcm-node.detune.value detune))
      (unless (equal? playback-rate 1.0)
        (set! pcm-node.playbackRate.value playback-rate))
      (when loop?
        (set! pcm-node.loop loop?)
        (set! pcm-node.loopStart loop-start)
        (set! pcm-node.loopEnd loop-end))
      (set! gain-node.gain.value gain)
      (pcm-node.connect gain-node)
      (gain-node.connect audioContext.destination)
      (pcm-node.start start-time)
      (pcm-node.stop end-time)
      (set! channel.lastPlaySec end-time)))))

;;;

(define *graviton-port* 0)
(define *graviton-access-log-drain* #t)
(define *graviton-error-log-drain* #t)
(define *control-channel* (make-server-control-channel))
(define *graviton-use-player?* #t)
(define *graviton-player-window-size* '(800 600))
(define *graviton-open-dev-tools?* #f)

(define (set-graviton-port! port)
  (set! *graviton-port* port))

(define (set-graviton-error-log-drain! log-drain)
  (set! *graviton-error-log* log-drain))

(define (set-graviton-use-player! flag)
  (set! *graviton-use-player?* flag))

(define (set-graviton-player-window-size! width height)
  (set! *graviton-player-window-size* (list width height)))

(define (set-graviton-open-dev-tools! flag)
  (set! *graviton-open-dev-tools?* flag))

(define (wsl?)
  (if (get-environment-variable "WSLENV")
      #t
      #f))

(define (player-executable-path)
  (let1 platform (gauche-config "--arch")
    (match-let1 (arch os) (string-split platform "-" 1)
      (cond
        ((and (#/x86_64/ arch)
              (#/linux/ os))
         (if (wsl?)
             "./player/graviton-player-win32-x64/graviton-player.exe"
             "./player/graviton-player-linux-x64/graviton-player"))
        ((and (#/x86_64/ arch)
              (#/mingw/ os))
         "./player/graviton-player-win32-x64/graviton-player.exe")
        (else
         (errorf "Unsupported platform: ~a" platform))))))

(define (invoke-player sock)
  (let* ((addr (socket-address sock))
         (config `((width . ,(list-ref *graviton-player-window-size* 0))
                   (height . ,(list-ref *graviton-player-window-size* 1))
                   (url . ,(format "http://localhost:~a/" (sockaddr-port addr)))
                   (background-color . ,*background-color*)
                   (open-dev-tools . ,*graviton-open-dev-tools?*)))
         (config-file (receive (out filename) (sys-mkstemp (build-path (temporary-directory) "grvcfg"))
                        (construct-json config out)
                        (close-port out)
                        (if (absolute-path? filename)
                            filename
                            (simplify-path (build-path (current-directory) filename)))))
         (player-path (player-executable-path)))
    (when (wsl?)
      (set! config-file (process-output->string `("wslpath" "-w" ,config-file))))
    (cond
      ((file-exists? player-path)
       (run-process `(,player-path "--config" ,config-file)))
      (else
       (run-process `("npx" "electron" "." "--config" ,config-file) :directory "./player/src")))))

(define (grv-main thunk)
  (set! *initial-thunk* thunk)
  (let ((port *graviton-port*))
    (start-http-server :port port
                       :access-log *graviton-access-log-drain*
                       :error-log *graviton-error-log-drain*
                       :control-channel *control-channel*
                       :startup-callback (lambda (socks)
                                           (run-scheduler)
                                           (when *graviton-use-player?*
                                             (invoke-player (car socks))))
                       :shutdown-callback (lambda ()
                                            (shutdown-scheduler!)))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_ expr ...)
     (grv-main (lambda () expr ...)))))
