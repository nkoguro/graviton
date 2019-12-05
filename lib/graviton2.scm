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
  (use gauche.hook)
  (use gauche.logger)
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.process)
  (use gauche.selector)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.time)
  (use gauche.uvector)
  (use makiki)
  (use rfc.base64)
  (use rfc.json)
  (use rfc.sha)
  (use srfi-13)
  (use srfi-19)
  (use srfi-27)
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

          async-apply
          async
          await
          asleep

          with-command-transaction
          app-close
          window-size
          make-canvas
          make-double-buffer-canvas
          load-canvas
          current-canvas
          set-canvas-visible!
          switch-double-buffer-canvas!

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
          free-image-data
          upload-image-data
          download-image-data

          set-window-event-handler!
          set-canvas-event-handler!

          set-audio-base-time!
          start-audio-node!
          stop-audio-node!
          connect-node!
          disconnect-node!
          free-node!
          audio-node-end
          make-audio-context-destination
          audio-param-set-value-at-time!
          audio-param-linear-ramp-to-value-at-time!
          audio-param-exponential-ramp-to-value-at-time!
          audio-param-set-target-at-time!
          audio-param-set-value-curve-at-time!
          audio-param-cancel-scheduled-values!
          audio-param-cancel-and-hold-at-time!
          pop-audio-param!
          make-oscillator-node
          set-oscillator-type!
          push-oscillator-frequency-audio-param!
          push-oscillator-detune-audio-param!
          set-oscillator-periodic-wave!
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

(define (make-id-generator :optional (max-value #f))
  (let1 counter-atom (atom 0)
    (lambda ()
      (atomic-update! counter-atom (lambda (x)
                                     (if max-value
                                         (modulo (+ x 1) max-value)
                                         (+ x 1)))))))

;;;

(define (register-event-handler! event-type proc pool out)
  (atomic *listener-table-atom*
    (lambda (tbl)
      (hash-table-set! tbl
                       event-type
                       (lambda (event)
                         (add-job! pool
                           (lambda ()
                             (reset
                               (guard (e (else
                                          (report-error e)
                                          (exit 70)))
                                 (parameterize ((main-thread-pool pool)
                                                (websocket-output-port out))
                                   (proc event)))))))))))

(define (unregister-event-handler! event-type)
  (atomic *listener-table-atom*
    (lambda (tbl)
      (hash-table-delete! tbl event-type))))

;;;

;; future is await-able only in the main thread.
(define-class <graviton-future> ()
  ((pool :init-form (main-thread-pool))
   (lock :init-form (make-mutex))
   (result-maker :init-value #f
                 :init-keyword :result-maker)
   (result-values :init-value #f)
   (result-exception :init-value #f)
   (continuations :init-value '())
   (ready? :init-value #f)))

(define (mark-future-ready! future)
  (slot-set! future 'ready? #t))

(define (call-future-result-maker future args)
  (set-future-result-values&exception! future (values->list (apply (slot-ref future 'result-maker) args)) #f))

(define (set-future-result-values&exception! future vals exception)
  (let ((lock (slot-ref future 'lock))
        (conts '()))
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
             (set! conts (slot-ref future 'continuations))
             (slot-set! future 'continuations '()))))
      (mutex-unlock! lock))
    (for-each (lambda (cont)
                (add-job! (slot-ref future 'pool)
                  (lambda ()
                    (reset
                      (cont vals exception)))))
              conts)))

(define (await future :optional (timeout #f) :rest timeout-vals)
  (unless (slot-ref future 'ready?)
    (errorf "~s hasn't been available yet." future))
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
       (receive (vals exception) (shift cont
                                   (push! (slot-ref future 'continuations) cont)
                                   (when timeout
                                     (add-schedule! (add-duration (current-time)
                                                                  (let* ((sec (floor timeout))
                                                                         (nanosec (round->exact (* (- timeout sec)
                                                                                                   1000000000))))
                                                                    (make-time time-duration nanosec sec)))
                                                    future
                                                    timeout-vals))
                                   (mutex-unlock! lock))
         (set! result-values vals)
         (set! result-exception exception))))
    (cond
      (result-values
       (if (null? result-values)
           (undefined)
           (apply values result-values)))
      (result-exception
       (raise result-exception))
      (else
       (error "[BUG] Invalid future state")))))

(define (asleep sec)
  (let1 future (make <graviton-future>)
    (mark-future-ready! future)
    (await future sec)))

(define (async-apply proc args)
  (let ((future (make <graviton-future>))
        (pool (main-thread-pool))
        (out (websocket-output-port)))
    (add-job! pool
      (lambda ()
        (reset
          (parameterize ((main-thread-pool pool)
                         (websocket-output-port out))
            (let ((result-values #f)
                  (result-exception #f))
              (guard (e (else (set! result-exception e)))
                (receive vals (apply proc args)
                  (set! result-values vals)))
              (set-future-result-values&exception! future result-values result-exception))))))
    (mark-future-ready! future)
    future))

(define-syntax async
  (syntax-rules ()
    ((_ expr ...)
     (async-apply (lambda () expr ...) '()))))

(define *future-table-atom* (atom (make-hash-table 'equal?)))
(define future-next-id (make-id-generator #xffffffff))
(define *listener-table-atom* (atom (make-hash-table 'equal?)))

(define (register-future! future)
  (atomic *future-table-atom*
    (lambda (tbl)
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
  (atomic *future-table-atom*
    (lambda (tbl)
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
          (else
           (errorf "[BUG] Invalid future ID: ~a" id)))))))

(define (notify-binary-result id)
  (atomic *future-table-atom*
    (lambda (tbl)
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

(define (notify-make-result id args)
  (atomic *future-table-atom*
    (lambda (tbl)
      (let1 future (hash-table-get tbl id #f)
        (cond
          (future
           (call-future-result-maker future (vector->list args))
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

(define (notify-event event-type-str event-alist)
  (atomic *listener-table-atom*
    (lambda (tbl)
      (and-let* ((event-type (string->symbol event-type-str))
                 (proc (hash-table-get tbl event-type #f)))
        (proc (if event-alist
                  (alist->event event-alist)
                  #f))))))

;;;

(define *scheduler-command-queue* (make-mtqueue))

(define (run-scheduler)
  (thread-start! (make-thread
                   (lambda ()
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
                                              (max (- (time->seconds (caar schedule-list)) (time->seconds now)) 0))
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
                                 (loop schedule-list)))))))))
                   "scheduler")))

(define (shutdown-scheduler!)
  (enqueue! *scheduler-command-queue* '(shutdown)))

(define (add-schedule! wake-time future vals)
  (enqueue! *scheduler-command-queue* (list 'schedule wake-time future vals)))

(define (cancel-schedule! future)
  (enqueue! *scheduler-command-queue* (list 'cancel future)))

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
                         (html:body :style (format "background-color: ~a" *background-color*)
                          (html:div :id "_on")
                          (html:script :src "graviton.js"))))))))

(define-http-handler "/graviton.js"
  (lambda (req app)
    (respond/ok req `(file ,*graviton-js-pathname*))))

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
(define main-thread-pool (make-parameter #f))
(define websocket-output-port (make-parameter #f))
(define *num-dispatcher* (atom 0))

(define json-command-table
  (alist->hash-table
    `(("notifyResult" . ,notify-result)
      ("notifyBinaryResult" . ,notify-binary-result)
      ("notifyMakeResult" . ,notify-make-result)
      ("notifyEvent" . ,notify-event))
    'equal?))

(define *binary-data-table-atom* (atom (make-hash-table 'equal?)))

(define (start-websocket-dispatcher! sock in out)
  (thread-start!
    (make-thread
      (lambda ()
        (atomic-update! *num-dispatcher* (^x (+ x 1)))
        (parameterize ((json-special-handler (lambda (sym)
                                               (case sym
                                                 ((false) #f)
                                                 ((true) #t)
                                                 (else sym)))))
          (guard (e (else (report-error e)))
            (let ((sel (make <selector>))
                  (ctx (make <websocket-server-context>))
                  (pool (make-thread-pool 1)))
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

              (selector-add! sel in (lambda (in flag)
                                      (while (and (byte-ready? in) (not (port-closed? in)))
                                        (dispatch-payload ctx
                                                          in
                                                          out
                                                          :text-handler receive-json
                                                          :binary-handler receive-binary
                                                          :close-handler close-websocket)))
                             '(r))
              (when *initial-thunk*
                (register-event-handler! 'start
                                         (lambda (event)
                                           (*initial-thunk*))
                                         pool
                                         out)
                (notify-event "start" #f))

              (while (not (port-closed? in))
                (selector-select sel))
              (terminate-all! pool :cancel-queued-jobs #t)
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
  (send-text-frame (websocket-output-port)
                   (construct-json-string (apply vector command-name args))))

(define binary-command-table
  (let1 tbl (make-hash-table 'eq?)
    (map-with-index (lambda (i cmd)
                      (hash-table-put! tbl cmd i))
                    '(
                      app-close
                      register-binary

                      make-canvas
                      load-canvas
                      set-canvas-visibility
                      window-size

                      set-fill-style
                      set-font
                      set-global-alpha
                      set-global-composite-operation
                      set-image-smoothing-enabled
                      set-line-cap
                      set-line-dash
                      set-line-dash-offset
                      set-line-join
                      set-line-width
                      set-miter-limit
                      set-shadow-blur
                      set-shadow-color
                      set-shadow-offset-x
                      set-shadow-offset-y
                      set-stroke-style
                      set-text-align
                      set-text-baseline
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
                      is-point-in-path
                      is-point-in-stroke
                      line-to
                      measure-text
                      move-to
                      put-image-data
                      quadratic-curve-to
                      rect
                      restore
                      rotate
                      save
                      scale
                      set-transform
                      stroke
                      stroke-rect
                      stroke-text
                      transform
                      translate
                      create-image-data
                      free-image-data
                      upload-image-data
                      download-image-data

                      listen-window-event
                      listen-canvas-event

                      set-audio-base-time
                      start-audio-node
                      stop-audio-node
                      connect-node
                      disconnect-node
                      free-node
                      audio-node-end
                      audio-param-set-value-at-time
                      audio-param-linear-ramp-to-value-at-time
                      audio-param-exponential-ramp-to-value-at-time
                      audio-param-set-target-at-time
                      audio-param-set-value-curve-at-time
                      audio-param-cancel-scheduled-values
                      audio-param-cancel-and-hold-at-time
                      pop-audio-param
                      make-audio-context-destination
                      make-oscillator-node
                      set-oscillator-type
                      push-oscillator-frequency-audio-param
                      push-oscillator-detune-audio-param
                      set-oscillator-periodic-wave
                      ))
    tbl))

(define-class <command-transaction> ()
  ((out :init-keyword :out)
   (object-pair-buffer :init-value '())
   (send-hook :init-keyword :send-hook)))

(define object-next-id (make-id-generator #xffffffff))

(define command-transaction (make-parameter #f))

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

(define (%call-command cmd types args)
  (let1 out (slot-ref (command-transaction) 'out)
    (write-u16 (hash-table-get binary-command-table cmd) out 'little-endian)
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
             ('boolean
               (write-u8 (if arg 1 0) out 'little-endian))
             ('object
              (let1 id (object-next-id)
                (push! (slot-ref (command-transaction) 'object-pair-buffer) (vector id arg))
                (write-u32 id out 'little-endian)))
             ('future
              (let1 id (register-future! arg)
                (write-u32 id out 'little-endian)
                (add-hook! (slot-ref (command-transaction) 'send-hook)
                  (lambda ()
                    (mark-future-ready! arg)))))
             ('canvas
              (write-u32 (slot-ref arg 'id) out 'little-endian))
             ('image
              (write-u32 (slot-ref arg 'id) out 'little-endian))
             (('resource content-type)
              (let* ((url (register-resource! content-type arg))
                     (id (object-next-id)))
                (push! (slot-ref (command-transaction) 'object-pair-buffer) (vector id url))
                (write-u32 id out 'little-endian)))
             ('audio-node
              (write-u32 (slot-ref arg 'id) out 'little-endian))))
         types args)))

(define (with-command-transaction thunk)
  (let* ((out (open-output-uvector))
         (txn (make <command-transaction> :out out :send-hook (make-hook))))
    (parameterize ((command-transaction txn))
      (begin0
        (thunk)
        (call-text-command "registerArgs" (list->vector (slot-ref txn 'object-pair-buffer)))
        (send-binary-frame (websocket-output-port)
                           (get-output-uvector out))
        (run-hook (slot-ref txn 'send-hook))))))

(define (call-command cmd types args)
  (if (command-transaction)
      (%call-command cmd types args)
      (with-command-transaction
        (lambda ()
          (%call-command cmd types args)))))

(define (window-size)
  (let* ((future (make <graviton-future>)))
    (call-command 'window-size '(future) (list future))
    future))

(define (app-close)
  (call-command 'app-close '() '()))

(define canvas-next-id (make-id-generator))

(define current-canvas (make-parameter #f))

(define-class <canvas> ()
  ((id :init-keyword :id)
   (width :init-keyword :width)
   (height :init-keyword :height)
   (z :init-keyword :z)
   (visible? :init-keyword :visible?)
   (context2d-list :init-form (list (make <context2d>)))
   (context2d :allocation :virtual
              :slot-ref (lambda (canvas)
                          (car (slot-ref canvas 'context2d-list))))))

(define-class <double-buffer-canvas> (<canvas>)
  ((canvases :init-keyword :canvases)
   (offscreen-index :init-value 0)
   (offscreen-canvas :allocation :virtual
                     :slot-ref (lambda (db-canvas)
                                 (vector-ref (slot-ref db-canvas 'canvases)
                                             (slot-ref db-canvas 'offscreen-index))))
   (onscreen-index :allocation :virtual
                   :slot-ref (lambda (db-canvas)
                               (modulo (+ (slot-ref db-canvas 'offscreen-index) 1) 2)))
   (onscreen-canvas :allocation :virtual
                    :slot-ref (lambda (db-canvas)
                                (vector-ref (slot-ref db-canvas 'canvases)
                                            (slot-ref db-canvas 'onscreen-index))))
   (id :allocation :virtual
       :slot-ref (lambda (db-canvas)
                   (slot-ref (slot-ref db-canvas 'offscreen-canvas) 'id)))
   (width :allocation :virtual
          :slot-ref (lambda (db-canvas)
                      (slot-ref (slot-ref db-canvas 'offscreen-canvas) 'width)))
   (height :allocation :virtual
          :slot-ref (lambda (db-canvas)
                      (slot-ref (slot-ref db-canvas 'offscreen-canvas) 'height)))
   (z :allocation :virtual
      :slot-ref (lambda (db-canvas)
                  (slot-ref (slot-ref db-canvas 'offscreen-canvas) 'z)))
   (visible? :allocation :virtual
             :slot-ref (lambda (db-canvas)
                         (slot-ref (slot-ref db-canvas 'onscreen-canvas) 'visible?)))
   (context2d-list :allocation :virtual
                   :slot-ref (lambda (db-canvas)
                               (slot-ref (slot-ref db-canvas 'offscreen-canvas) 'context2d-list)))))

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

(define image-next-id (make-id-generator))

(define-class <graviton-image> ()
  ((id :init-form (image-next-id))
   (width :init-keyword :width)
   (height :init-keyword :height)
   (valid? :init-value #t)))

(define (invalidate obj)
  (slot-set! obj 'valid? #f))

(define (valid? obj)
  (slot-ref obj 'valid?))

(define (check-valid obj)
  (unless (valid? obj)
    (errorf "~s isn't available." obj)))

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
                   (slot-ref canvas 'id)))
    ("image" . ,(and-let1 image (slot-ref pattern 'image)
                  (slot-ref image 'id)))
    ("repetition" . ,(slot-ref pattern 'repetition))))

(define (make-canvas width height :key (z 0) (visible? #t))
  (let ((id (canvas-next-id)))
    (call-command 'make-canvas '(u32 u32 u32 u32 boolean) (list id width height z visible?))
    (let1 canvas (make <canvas> :id id :width width :height height :z z :visible? visible?)
      (when visible?
        (current-canvas canvas))
      canvas)))

(define (load-canvas filename :key (z 0) (visible? #t) (content-type #f))
  (let* ((id (canvas-next-id))
         (future (make <graviton-future> :result-maker (lambda (w h)
                                                         (make <canvas>
                                                           :id id
                                                           :width w
                                                           :height h
                                                           :z z
                                                           :visible? visible?))))
         (data (call-with-input-file filename port->uvector)))
    (call-command 'load-canvas
                  `(future
                    u32
                    (resource ,(or content-type
                                   (estimate-content-type filename)))
                    u32
                    boolean)
                  (list future id data z visible?))
    future))

(define (make-double-buffer-canvas width height :key (z 0) (visible? #t))
  (let ((onscreen-canvas (make-canvas width height :z z :visible? visible?))
        (offscreen-canvas (make-canvas width height :z z :visible? #f)))
    (let1 db-canvas (make <double-buffer-canvas> :canvases (vector offscreen-canvas onscreen-canvas))
      (when visible?
        (current-canvas db-canvas))
      db-canvas)))

(define (switch-double-buffer-canvas! db-canvas)
  (let ((onscreen-canvas (slot-ref db-canvas 'onscreen-canvas))
        (offscreen-canvas (slot-ref db-canvas 'offscreen-canvas)))
    (set-canvas-visible! offscreen-canvas (slot-ref onscreen-canvas 'visible?))
    (set-canvas-visible! onscreen-canvas #f)
    (slot-set! db-canvas
               'offscreen-index
               (modulo (+ (slot-ref db-canvas 'offscreen-index) 1) 2))))

(define (set-canvas-visible! canvas visible?)
  (call-command 'set-canvas-visibility '(canvas boolean) (list canvas visible?))
  (slot-set! canvas 'visible? visible?))

(define (current-fill-style)
  (~ (current-canvas) 'context2d 'fill-style))

(define (set-fill-style! style)
  (set! (~ (current-canvas) 'context2d 'fill-style) style)
  (call-command 'set-fill-style '(canvas object) (list (current-canvas) (style->json style))))

(define (current-font)
  (~ (current-canvas) 'context2d 'font))

(define (set-font! font)
  (set! (~ (current-canvas) 'context2d 'font) font)
  (call-command 'set-font '(canvas object) (list (current-canvas) font)))

(define (current-global-alpha)
  (~ (current-canvas) 'context2d 'global-alpha))

(define (set-global-alpha! alpha)
  (set! (~ (current-canvas) 'context2d 'global-alpha) alpha)
  (call-command 'set-global-alpha '(canvas f64) (list (current-canvas) alpha)))

(define (current-global-composite-operation)
  (~ (current-canvas) 'context2d 'global-composite-operation))

(define-constant global-composite-opration-alist
  '((source-over . "source-over")
    (source-in . "source-in")
    (source-out . "source-out")
    (source-atop . "source-atop")
    (destination-over . "destination-over")
    (destination-in . "destination-in")
    (destination-out . "destination-out")
    (destination-atop . "destination-atop")
    (lighter . "lighter")
    (copy . "copy")
    (xor . "xor")
    (multiply . "multiply")
    (screen . "screen")
    (overlay . "overlay")
    (darken . "darken")
    (lighten . "lighten")
    (color-dodge . "color-dodge")
    (color-burn . "color-burn")
    (hard-light . "hard-light")
    (soft-light . "soft-light")
    (difference . "difference")
    (exclusion . "exclusion")
    (hue . "hue")
    (saturation . "saturation")
    (color . "color")
    (luminosity . "luminosity")))

(define (set-global-composite-operation! op)
  (let1 op-name (assoc-ref global-composite-opration-alist op #f)
    (unless op-name
      (errorf "Invalid global composite operation: ~a" op))
    (set! (~ (current-canvas) 'context2d 'global-composite-opration) op)
    (call-command 'set-global-composite-operation '(canvas object) (list (current-canvas) op-name))))

(define (current-image-smoothing-enabled?)
  (~ (current-canvas) 'context2d 'image-smoothing-enabled))

(define (set-image-smoothing-enabled! flag)
  (set! (~ (current-canvas) 'context2d 'image-smoothing-enabled) flag)
  (call-command 'set-image-smoothing-enabled '(canvas boolean) (list (current-canvas) flag)))

(define (current-line-cap)
  (~ (current-canvas) 'context2d 'line-cap))

(define-constant line-cap-alist
  '((butt . "butt")
    (round . "round")
    (square . "square")))

(define (set-line-cap! opt)
  (let1 opt-name (assoc-ref line-cap-alist opt #f)
    (unless opt-name
      (errorf "Invalid line cap option: ~a" opt))
    (set! (~ (current-canvas) 'context2d 'line-cap) opt)
    (call-command 'set-line-cap '(canvas object) (list (current-canvas) opt-name))))

(define (current-line-dash)
  (~ (current-canvas) 'context2d 'line-dash))

(define (set-line-dash! segments)
  (set! (~ (current-canvas) 'context2d 'line-dash) segments)
  (call-command 'set-line-dash '(canvas object) (list (current-canvas) segments)))

(define (current-line-dash-offset)
  (~ (current-canvas) 'context2d 'line-dash-offset))

(define (set-line-dash-offset! offset)
  (set! (~ (current-canvas) 'context2d 'line-dash-offset) offset)
  (call-command 'set-line-dash-offset '(canvas f64) (list (current-canvas) offset)))

(define (current-line-join)
  (~ (current-canvas) 'context2d 'line-join))

(define-constant line-join-alist
  '((bevel . "bevel")
    (round . "round")
    (miter . "miter")))

(define (set-line-join! opt)
  (let1 opt-name (assoc-ref line-join-alist opt #f)
    (unless opt-name
      (errorf "Invalid line join option: ~a" opt))
    (set! (~ (current-canvas) 'context2d 'line-join) opt)
    (call-command 'set-line-join '(canvas object) (list (current-canvas) offset))))

(define (current-line-width)
  (~ (current-canvas) 'context2d 'line-width))

(define (set-line-width! w)
  (set! (~ (current-canvas) 'context2d 'line-width) w)
  (call-command 'set-line-width '(canvas f64) (list (current-canvas) w)))

(define (current-miter-limit)
  (~ (current-canvas) 'context2d 'miter-limit))

(define (set-miter-limit! limit)
  (set! (~ (current-canvas) 'context2d 'miter-limit) limit)
  (call-command 'set-miter-limit '(canvas f64) (list (current-canvas) limit)))

(define (current-shadow-blur)
  (~ (current-canvas) 'context2d 'shadow-blur))

(define (set-shadow-blur! level)
  (set! (~ (current-canvas) 'context2d 'shadow-blur) level)
  (call-command 'set-shadow-blur '(canvas f64) (list (current-canvas) level)))

(define (current-shadow-color)
  (~ (current-canvas) 'context2d 'shadow-color))

(define (set-shadow-color! color)
  (set! (~ (current-canvas) 'context2d 'shadow-color) color)
  (call-command 'set-shadow-color '(canvas object) (list (current-canvas) color)))

(define (current-shadow-offset-x)
  (~ (current-canvas) 'context2d 'shadow-offset-x))

(define (set-shadow-offset-x! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-x) offset)
  (call-command 'set-shadow-offset-x '(canvas f64) (list (current-canvas) offset)))

(define (current-shadow-offset-y)
  (~ (current-canvas) 'context2d 'shadow-offset-y))

(define (set-shadow-offset-y! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-y) offset)
  (call-command 'set-shadow-offset-y '(canvas f64) (list (current-canvas) offset)))

(define (current-stroke-style)
  (~ (current-canvas) 'context2d 'stroke-style))

(define (set-stroke-style! style)
  (set! (~ (current-canvas) 'context2d 'stroke-style) style)
  (call-command 'set-stroke-style '(canvas object) (list (current-canvas) (style->json style))))

(define (current-text-align)
  (~ (current-canvas) 'context2d 'text-align))

(define-constant text-align-alist
  '((left . "left")
    (right . "right")
    (center . "center")
    (start . "start")
    (end . "end")))

(define (set-text-align! align)
  (let1 align-name (assoc-ref text-align-alist align #f)
    (unless align-name
      (errorf "Invalid text align option: ~a" align))
    (set! (~ (current-canvas) 'context2d 'text-align) align)
    (call-command 'set-text-align '(canvas object) (list (current-canvas) align-name))))

(define (current-text-baseline)
  (~ (current-canvas) 'context2d 'text-baseline))

(define-constant text-baseline-alist
  '((top . "top")
    (hanging . "hanging")
    (middle . "middle")
    (alphabetic . "alphabetic")
    (ideographic . "ideographic")
    (bottom . "bottom")))

(define (set-text-baseline! opt)
  (let1 opt-name (assoc-ref text-baseline-alist opt #f)
    (unless opt-name
      (errorf "Invalid text baseline option: ~a" opt))
    (set! (~ (current-canvas) 'context2d 'text-baseline) opt)
    (call-command 'set-text-baseline '(canvas object) (list (current-canvas) opt-name))))

(define (arc x y radius start-angle end-angle :optional (anti-clockwise #f))
  (call-command 'arc
                '(canvas s32 s32 s32 f64 f64 boolean)
                (list (current-canvas) x y radius start-angle end-angle anti-clockwise)))

(define (arc-to x1 y1 x2 y2 radius)
  (call-command 'arc-to '(canvas s32 s32 s32 s32 s32) (list (current-canvas) x1 y1 x2 y2 radius)))

(define (begin-path)
  (call-command 'begin-path '(canvas) (list (current-canvas))))

(define (bezier-curve-to cp1x cp1y cp2x cp2y x y)
  (call-command 'bezier-curve-to '(canvas s32 s32 s32 s32 s32 s32) (list (current-canvas) cp1x cp1y cp2x cp2y x y)))

(define (clear-rect x y w h)
  (call-command 'clear-rect '(canvas s32 s32 s32 s32) (list (current-canvas) x y w h)))

(define fillrule-alist
  '((nonzero . "nonzero")
    (evenodd . "evenodd")))

(define (clip :optional (rule 'nonzero))
  (let1 rule-name (assoc-ref fillrule-alist rule #f)
    (unless rule-name
      (errorf "Invalid fillrule: ~a" rule))
    (call-command 'clip '(canvas object) (list (current-canvas) rule-name))))

(define (close-path)
  (call-command 'close-path '(canvas) (list (current-canvas))))

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
      (else
       #f))))

(define-method draw-canvas ((canvas <canvas>) dx dy)
  (call-command 'draw-canvas '(canvas canvas u8 s32 s32) (list (current-canvas) canvas 3 dx dy)))

(define-method draw-canvas ((canvas <canvas>) dx dy dw dh)
  (call-command 'draw-canvas '(canvas canvas u8 s32 s32 s32 s32) (list (current-canvas) canvas 2 dx dy dw dh)))

(define-method draw-canvas ((canvas <canvas>) sx sy sw sh dx dy dw dh)
  (call-command 'draw-canvas
                '(canvas canvas u8 s32 s32 s32 s32 s32 s32 s32 s32)
                (list (current-canvas) canvas 1 sx sy sw sh dx dy dw dh)))

(define (ellipse x y radius-x radius-y rotation start-angle end-angle :optional (anti-clockwise #f))
  (call-command 'ellipse
                '(canvas s32 s32 s32 s32 f64 f64 f64 boolean)
                (list (current-canvas) x y radius-x radius-y rotation start-angle end-angle anti-clockwise)))

(define (fill :optional (rule 'nonzero))
  (let1 rule-name (assoc-ref fillrule-alist rule #f)
    (unless rule-name
      (errorf "Invalid fillrule: ~a" rule))
    (call-command 'fill '(canvas object) (list (current-canvas) rule-name))))

(define (fill-rect x y w h)
  (call-command 'fill-rect '(canvas s32 s32 s32 s32) (list (current-canvas) x y w h)))

(define (fill-text text x y :optional (max-width 0))
  (call-command 'fill-text '(canvas object s32 s32 s32) (list (current-canvas) text x y max-width)))

(define (get-image-data sx sy sw sh)
  (let1 image (make <graviton-image> :width sw :height sh)
    (call-command 'get-image-data '(canvas image s32 s32 s32 s32) (list (current-canvas) image sx sy sw sh))
    image))

(define (is-point-in-path? x y :optional (rule 'nonzero))
  (let1 rule-name (assoc-ref fillrule-alist rule #f)
    (unless rule-name
      (errorf "Invalid fillrule: ~a" rule))

    (let1 future (make <graviton-future>)
      (call-command 'is-point-in-path '(canvas future s32 s32 object) (list (current-canvas) future x y rule-name))
      future)))

(define (is-point-in-stroke? x y)
  (let1 future (make <graviton-future>)
    (call-command 'is-point-in-stroke '(canvas future s32 s32) (list (current-canvas) future x y))
    future))

(define (line-to x y)
  (call-command 'line-to '(canvas s32 s32) (list (current-canvas) x y)))

(define-class <text-metrics> ()
  ((width :init-keyword :width)))

(define (measure-text text)
  (let1 future (make <graviton-future> :result-maker (lambda (alist)
                                                       (make <text-metrics> :width (assoc-ref alist "width"))))
    (call-command 'measure-text '(canvas future object) (list (current-canvas) future text))
    future))

(define (move-to x y)
  (call-command 'move-to '(canvas s32 s32) (list (current-canvas) x y)))

(define-method put-image-data ((image <graviton-image>) dx dy)
  (check-valid image)
  (call-command 'put-image-data '(canvas image s32 s32 boolean) (list (current-canvas) image dx dy #f)))

(define-method put-image-data ((image <graviton-image>) dx dy dirty-x dirty-y dirty-width dirty-height)
  (check-valid image)
  (call-command 'put-image-data
                '(canvas image s32 s32 boolean s32 s32 s32 s32)
                (list (current-canvas) image #f dx dy dirty-x dirty-y dirty-width dirty-height)))

(define (quadratic-curve-to cpx cpy x y)
  (call-command 'quadratic-curve-to '(canvas s32 s32 s32 s32) (list (current-canvas) cpx cpy x y)))

(define (rect x y w h)
  (call-command 'rect '(canvas s32 s32 s32 s32) (list (current-canvas) x y w h)))

(define (restore-context)
  (call-command 'restore '(canvas) (list (current-canvas)))
  (pop! (slot-ref (current-canvas) 'context2d-list)))

(define (rotate angle)
  (call-command 'rotate '(canvas f64) (list (current-canvas) angle)))

(define (save-context)
  (call-command 'save '(canvas) (list (current-canvas)))
  (let1 ctx2 (copy-context2d (slot-ref (current-canvas) 'context2d))
    (push! (slot-ref (current-canvas) 'context2d-list) ctx2)))

(define (scale x y)
  (call-command 'scale '(canvas f64 f64) (list (current-canvas) x y)))

(define (set-transform! a b c d e f)
  (call-command 'set-transform '(canvas f64 f64 f64 f64 f64 f64) (list (current-canvas) a b c d e f)))

(define (stroke)
  (call-command 'stroke '(canvas) (list (current-canvas))))

(define (stroke-rect x y w h)
  (call-command 'stroke-rect '(canvas s32 s32 s32 s32) (list (current-canvas) x y w h)))

(define (stroke-text text x y :optional (max-width 0))
  (call-command 'stroke-text '(canvas object s32 s32 s32) (list (current-canvas) text x y max-width)))

(define (transform a b c d e f)
  (call-command 'transform '(canvas f64 f64 f64 f64 f64 f64) (list (current-canvas) a b c d e f)))

(define (translate x y)
  (call-command 'translate '(canvas s32 s32) (list (current-canvas) x y)))

(define (create-image-data w h)
  (let1 image (make <graviton-image> :width w :height h)
    (call-command 'create-image-data '(canvas image s32 s32) (list (current-canvas) image w h))
    image))

(define (free-image-data image)
  (call-command 'free-image-data '(image) image)
  (invalidate image))

(define (upload-image-data image data)
  (check-valid image)
  (call-command 'upload-image-data '(image u8vector) (list image data)))

(define (download-image-data image)
  (check-valid image)
  (let1 future (make <graviton-future>)
    (call-command 'download-image-data '(future image) (list future image))
    future))

(define (set-window-event-handler! event-type proc)
  (call-command 'listen-window-event '(object boolean) (list (symbol->string event-type) (if proc #t #f)))
  (cond
    (proc
     (register-event-handler! event-type proc (main-thread-pool) (websocket-output-port)))
    (else
     (unregister-event-handler! event-type))))

(define (set-canvas-event-handler! canvas event-type proc)
  (let1 event-name (format "_canvas_~a_~a" (slot-ref canvas 'id) event-type)
    (call-command 'listen-canvas-event
                  '(canvas object object boolean)
                  (list canvas (symbol->string event-type) event-name (if proc #t #f)))
    (cond
      (proc
       (register-event-handler! (string->symbol event-name) proc (main-thread-pool) (websocket-output-port)))
      (else
       (unregister-event-handler! (string->symbol event-name))))))

;;;

(define audio-node-next-id (make-id-generator #xffffffff))

(define-class <audio-node> ()
  ((id :init-form (image-next-id))
   (type :init-keyword :type)))

(define (set-audio-base-time!)
  (call-command 'set-audio-base-time '() '()))

(define (start-audio-node! audio-node :optional (delta-when 0) (offset 0) (duration -1))
  (call-command 'start-audio-node '(audio-node f64 f64 f64) (list audio-node delta-when offset duration)))

(define (stop-audio-node! audio-node :optional (delta-when 0))
  (call-command 'stop-audio-node '(audio-node f64) (list audio-node delta-when)))

(define (connect-node! from-node to-node)
  (call-command 'connect-node '(audio-node audio-node) (list from-node to-node)))

(define (disconnect-node! from-node to-node)
  (call-command 'disconnect-node '(audio-node audio-node) (list from-node to-node)))

(define (free-node! audio-node)
  (call-command 'free-node '(audio-node) (list audio-node)))

(define (audio-node-end audio-node)
  (let1 future (make <graviton-future>)
    (call-command 'audio-node-end '(future audio-node) (list future audio-node))
    future))

(define (make-audio-context-destination)
  (let1 audio-node (make <audio-node> :type 'audio-context-destination)
    (call-command 'make-audio-context-destination '(audio-node) (list audio-node))
    audio-node))

(define (audio-param-set-value-at-time! val start-time)
  (call-command 'audio-param-set-value-at-time '(f64 f64) (list val start-time)))

(define (audio-param-linear-ramp-to-value-at-time! val end-time)
  (call-command 'audio-param-linear-ramp-to-value-at-time '(f64 f64) (list val end-time)))

(define (audio-param-exponential-ramp-to-value-at-time! val end-time)
  (call-command 'audio-param-exponential-ramp-to-value-at-time '(f64 f64) (list val end-time)))

(define (audio-param-set-target-at-time! val start-time time-constant)
  (call-command 'audio-param-set-target-at-time '(f64 f64 f64) (list val start-time time-constant)))

(define (audio-param-set-value-curve-at-time! vals start-time duration)
  (call-command 'audio-param-set-value-curve-at-time '(object f64 f64) (list vals start-time duration)))

(define (audio-param-cancel-scheduled-values! start-time)
  (call-command 'audio-param-cancel-scheduled-values '(f64) (list start-time)))

(define (audio-param-cancel-and-hold-at-time! cancel-time)
  (call-command 'audio-param-cancel-and-hold-at-time '(f64) (list cancel-time)))

(define (pop-audio-param!)
  (call-command 'pop-audio-param '() '()))

(define (make-oscillator-node)
  (let1 oscillator-node (make <audio-node> :type 'oscillator)
    (call-command 'make-oscillator-node '(audio-node) (list oscillator-node))
    oscillator-node))

(define oscillator-type-alist
  '((sine . "sine")
    (square . "square")
    (sawtooth . "sawtooth")
    (triangle . "triangle")))

(define (set-oscillator-type! oscillator-node type)
  (call-command 'set-oscillator-type '(audio-node object) (list oscillator-node
                                                                (or (assoc-ref oscillator-type-alist type #f)
                                                                    (errorf "Invalid oscillator type: ~a" type)))))

(define (push-oscillator-frequency-audio-param! oscillator-node)
  (call-command 'push-oscillator-frequency-audio-param '(audio-node) (list oscillator-node)))

(define (push-oscillator-detune-audio-param! oscillator-node)
  (call-command 'push-oscillator-detune-audio-param '(audio-node) (list oscillator-node)))

(define (set-oscillator-periodic-wave! oscillator-node real imag :key (disable-nomalization #f))
  (call-command 'set-oscillator-periodic-wave
                '(audio-node object object object)
                (list oscillator-node real imag `(("disableNomalization" . ,disable-nomalization)))))

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

(define (invoke-player sock)
  (let* ((addr (socket-address sock))
         (config `((width . ,(list-ref *graviton-player-window-size* 0))
                   (height . ,(list-ref *graviton-player-window-size* 1))
                   (url . ,(format "http://~a" (sockaddr-name addr)))
                   (background-color . ,*background-color*)
                   (open-dev-tools . ,*graviton-open-dev-tools?*)))
         (config-file (receive (out filename) (sys-mkstemp (build-path (temporary-directory) "grvcfg"))
                        (construct-json config out)
                        (close-port out)
                        (if (absolute-path? filename)
                            filename
                            (simplify-path (build-path (current-directory) filename))))))
    (run-process `("npx" "electron" "." "--config" ,config-file) :directory "./player")))

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
