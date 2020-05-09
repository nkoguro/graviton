;;;
;;; graviton.scm - Graviton
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

(define-module graviton
  (use binary.io)
  (use control.thread-pool)
  (use data.queue)
  (use file.util)
  (use gauche.charconv)
  (use gauche.collection)
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
  (use graviton.config)
  (use graviton.jsise)
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
          asleep

          command-buffering?
          set-command-buffering?
          flush-commands
          app-close
          window-size
          unlink-proxy-object!

          current-thread-pool
          submit-thunk

          define-action

          application-context
          application-context-slot-atomic-ref
          application-context-slot-atomic-update!
          application-context-slot-ref
          application-context-slot-set!
          define-application-context-slot

          browser-window

          <proxy-object>
          proxy-object-id
          define-jsenum
          jsrun
          jscall
          jslambda
          jslet
          jslet/result
          jslet/result:then
          estimate-content-type
          resource-url

          loop-frame))

(select-module graviton)

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
    (flush-commands)
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
                               (Graviton.unlinkProxyObject id))))
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

(define *js-path-map-alist* '())

(define (register-js-path-map file-path url-path)
  (set! *js-path-map-alist* (append *js-path-map-alist*
                                    (list
                                      (cons (simplify-path (string-append "/" url-path "/"))
                                            file-path)))))

(register-js-path-map (graviton-js-directory) "graviton")

(define (resolve-js-path js-path)
  (let1 js-path (simplify-path js-path)
    (let loop ((path-map-alist *js-path-map-alist*))
      (match path-map-alist
        (()
         #f)
        (((url-path . file-path) rest ...)
         (or (and-let1 path (and (string-prefix? url-path js-path)
                                 (build-path file-path (string-drop js-path (string-length url-path))))
               (and (file-is-readable? path)
                    path))
             (loop rest)))))))

(define-http-handler #/\/js(\/.*)/
  (lambda (req app)
    (let1 js-path ((slot-ref req 'path-rxmatch) 1)
      (cond
        ((resolve-js-path js-path)
         => (lambda (file-path)
              (respond/ok req `(file ,file-path) :content-type (estimate-content-type js-path))))
        ((and (equal? (sys-dirname js-path) "/")
              (get-js-code (sys-basename js-path)))
         => (lambda (js-code)
              (respond/ok req js-code :content-type (estimate-content-type js-path))))
        (else
         (respond/ng req 404))))))

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
                                (append
                                  (map (lambda (js-path)
                                         (html:script :src js-path))
                                       (load-js-list))
                                  (map (lambda (js-mod)
                                         (html:script :type "module" :src js-mod))
                                       (js-main-module-absolute-paths))))))))))

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
   (proxy-object-id-generator :init-form (make-id-generator #xffffffff))
   (command-buffering? :init-value #f)))

(define *application-context-slot-initials* '())

(define-syntax define-application-context-slot
  (syntax-rules ()
    ((_ name val)
     (push! *application-context-slot-initials* (cons 'name val)))))

(define-class <application-context> ()
  ((main-thread-pool :init-keyword :main-thread-pool)
   (worker-thread-pool :init-keyword :worker-thread-pool)
   (send-context-atom :init-keyword :send-context-atom)
   (future-table-atom :init-form (atom (make-hash-table 'equal?) (make-id-generator #xffffffff)))
   (slot-table-atom :init-form (atom (let1 tbl (make-hash-table 'eq?)
                                       (for-each (match-lambda
                                                   ((name . val)
                                                    (hash-table-put! tbl name val)))
                                                 *application-context-slot-initials*)
                                       tbl)))))

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

(define (application-context-slot-atomic-ref name proc)
  (atomic (slot-ref (application-context) 'slot-table-atom)
    (lambda (tbl)
      (proc (hash-table-get tbl name #f)))))

(define (application-context-slot-ref name)
  (atomic (slot-ref (application-context) 'slot-table-atom)
    (lambda (tbl)
      (hash-table-get tbl name #f))))

(define (application-context-slot-atomic-update! name proc)
  (atomic (slot-ref (application-context) 'slot-table-atom)
    (lambda (tbl)
      (let1 val (proc (hash-table-get tbl name #f))
        (hash-table-put! tbl name val)
        val))))

(define (application-context-slot-set! name val)
  (application-context-slot-atomic-update! name (lambda _ val)))

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

(define (command-buffering?)
  (with-send-context
    (lambda (ctx)
      (slot-ref ctx 'command-buffering?))))

(define (set-command-buffering? val)
  (with-send-context
    (lambda (ctx)
      (slot-set! ctx 'command-buffering? val))))

(define (proxy-object-next-id)
  (with-send-context (lambda (ctx)
                       ((slot-ref ctx 'proxy-object-id-generator)))))

(define (with-future-table proc)
  (atomic (slot-ref (application-context) 'future-table-atom) proc))

(define json-command-table
  (alist->hash-table
    `(("notifyResult" . ,notify-result))
    'equal?))

(define-syntax define-action
  (syntax-rules ()
    ((_ action-name args body ...)
     (hash-table-put! json-command-table action-name (lambda args body ...)))))

(define-action "startApplication" ()
  (when *initial-thunk*
    (submit-thunk (main-thread-pool)
      (lambda ()
        (run-hook *init-hook*)
        (*initial-thunk*)))))

(define *binary-data-table-atom* (atom (make-hash-table 'equal?)))

(define (notify-binary-result id data)
  (with-future-table
    (lambda (tbl future-next-id)
      (let1 future (begin0
                     (hash-table-get tbl id #f)
                     (hash-table-delete! tbl id))
        (cond
          (future
           (set-future-result-values&exception! future (list data) #f))
          (else
           (errorf "[BUG] Invalid future ID: ~a" id)))))))

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
              (let* ((id (get-u32 data 0))
                     (result (uvector-alias <u8vector> data 4)))
                (notify-binary-result id result)))

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

              (while (not (port-closed? in))
                (selector-select sel))

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
      (else
       #f))))

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
                 ('future
                  (let1 id (register-future! arg)
                    (write-u32 id out 'little-endian)))
                 ('proxy
                  (write-u32 (proxy-object-id arg) out 'little-endian))
                 ('string
                   (let1 data (ces-convert-to <u8vector> arg 'utf-8)
                     (write-u32 (u8vector-length data) out 'little-endian)
                     (write-uvector data out )))
                 ('json
                  ;; Make a vector to make JSON to satisfy RFC4627.
                  (let1 data (ces-convert-to <u8vector> (construct-json-string (vector arg)) 'utf-8)
                    (write-u32 (u8vector-length data) out 'little-endian)
                    (write-uvector data out)))
                 ((? symbol? enum-name)
                  (unless (hash-table-contains? *enum-table* enum-name)
                    (errorf "Invalid type: ~a" type))
                  (write-u8 (enum-value enum-name arg) out 'little-endian))))
             types args))
      (unless (command-buffering?)
        (flush-commands)))))

(define (flush-commands)
  (with-send-context
    (lambda (ctx)
      (let1 output-data (get-output-uvector (slot-ref ctx 'buffer-output-port))
        (unless (= (uvector-length output-data) 0)
          (send-binary-frame (slot-ref ctx 'websocket-output-port) output-data)))
      (slot-set! ctx 'buffer-output-port (open-output-uvector)))))

;;;

(define (parse-arg-spec spec)
  (match spec
    ((? symbol? spec)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")))))
    (((? symbol? spec))
     (let1 parts (map string->symbol (string-split (symbol->string spec) "::"))
       (cond
         ((= (length parts) 1)
          (parse-arg-spec (append parts '(json))))
         (else
          (parse-arg-spec (list parts))))))
    (((? symbol? spec) init-val)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")) init-val)))
    (((var type) init-val)
     (list var type init-val))
    (((var type))
     (list var type var))
    (((var) init-val)
     (list var 'json init-val))))

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
                                         ((var type _)
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
                                                    ('string
                                                      `((ref ,ds getString)))
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
                          (Graviton.registerBinaryCommand ,call-id ,name)
                          (Graviton.registerBinaryCommand ,run-id ,name)))
    (make <js-procedure> :call-id call-id :run-id run-id :types (map (cut list-ref <> 1) var-type-list))))

(define-jsise-macro result
  ((vals ...)
   `(Graviton.notifyValues %future-id ,(list->vector vals))))

(define-jsise-macro result-u8array
  ((data)
   `(Graviton.notifyBinaryData %future-id ,data)))

(define-jsise-macro result-error
  ((err)
   `(Graviton.notifyException %future-id ((ref ,err toString)))))

(define-macro (jslambda arg-specs :rest body)
  (compile-jslambda (js-vm-current-main-module) arg-specs body))

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
                   (list-ref (parse-arg-spec spec) 2))
                 var-spec)))

(define-macro (jslet/result var-spec :rest body)
  `(jscall (jslambda ,var-spec
             ,@body)
           #f
           ,@(map (lambda (spec)
                    (list-ref (parse-arg-spec spec) 2))
                  var-spec)))

(define-macro (jslet/result:then provider var-spec :rest body)
  `(jscall (jslambda ,var-spec
             ,@body)
           ,provider
           ,@(map (lambda (spec)
                    (list-ref (parse-arg-spec spec) 2))
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
                       `(Graviton.registerEnum ,(symbol->string enum-name) ,(list->vector (reverse jsvals))))
    enum))

(define-syntax define-jsenum
  (syntax-rules ()
    ((_ name symbol->jsvalue ...)
     (let1 enum (make-enum (js-current-main-module) 'name '(symbol->jsvalue ...))
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

(define-class <browser-window> (<proxy-object>)
  ())

(define (browser-window)
  (application-context-slot-atomic-update! 'browser-window
    (lambda (win)
      (unless win
        (set! win (make <browser-window>))
        (let1 proxy-id (proxy-object-id win)
          (jslet (proxy-id::u32)
            (Graviton.linkProxyObject proxy-id window))))
      win)))

(define (window-size)
  (jslet/result ()
    (result window.innerWidth window.innerHeight)))

(define (app-close)
  (jslet ()
    (Graviton.closeConnection)))

;;;

(define (loop-frame proc :key (fps 30))
  (let ((exit? #f)
        (frame-sec (/. 1 fps))
        (current-buffering-mode (command-buffering?))
        (prev-sec 0))
    (define (break)
      (set! exit? #t))
    (set-command-buffering? #t)
    (unwind-protect
        (while (not exit?)
          (let1 start-sec (time->seconds (current-time))
            (proc break)
            (flush-commands)
            (let* ((elapse-sec (- (time->seconds (current-time)) start-sec)))
              (when (< frame-sec elapse-sec)
                (log-debug "Frame dropped. renderer procedure consumes ~a sec. It exceeds one frame sec: ~a"
                           elapse-sec
                           frame-sec))
              (asleep (max 0 (- frame-sec elapse-sec))))))
      (set-command-buffering? current-buffering-mode))))

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
                   (url . ,(format "http://localhost:~a/" (sockaddr-port addr)))
                   (background-color . ,*background-color*)
                   (open-dev-tools . ,*graviton-open-dev-tools?*)))
         (config-file (receive (out filename) (sys-mkstemp (build-path (temporary-directory) "grvcfg"))
                        (construct-json config out)
                        (close-port out)
                        (if (absolute-path? filename)
                            filename
                            (simplify-path (build-path (current-directory) filename)))))
         (player-path (graviton-config 'graviton-player-path)))
    (when (graviton-config 'wsl?)
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
