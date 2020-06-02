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
  (use gauche.record)
  (use gauche.regexp)
  (use gauche.selector)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.time)
  (use gauche.uvector)
  (use graviton.async)
  (use graviton.config)
  (use graviton.context)
  (use graviton.jsise)
  (use graviton.scheduler)
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

  (export grv-main
          grv-begin

          grv-player
          grv-browser
          grv-log-config

          make-task-queue
          transform-future
          async-apply
          async
          await
          asleep
          task-yield
          make-channel
          channel-send
          channel-recv
          channel-recv/await
          channel-close

          command-buffering?
          set-command-buffering?
          flush-commands
          app-close
          window-size
          app-close-hook

          current-task-queue

          define-action

          browser-window

          <jsobject>
          jsobject-id
          invalidate!
          invalidate?
          define-jsenum
          jslet
          jslet/result

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

;;;

(log-open #t)

(define (log-debug fmt :rest args)
  (when (<= (log-level) 0)
    (apply log-format fmt args)))

(define (log-info fmt :rest args)
  (when (<= (log-level) 1)
    (apply log-format fmt args)))

(define (log-error fmt :rest args)
  (when (<= (log-level) 2)
    (apply log-format fmt args)))

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

(define-http-handler "/"
  (lambda (req app)
    (respond/ok req (tree->string
                      (html:html
                       (html:head
                        (html:meta :charset "UTF-8")
                        (html:link :rel "icon" :href "data:png;base64,")
                        (html:title (client-title)))
                       (apply html:body :style (format "background-color: ~a" (client-background-color))
                              (append
                                (map (lambda (js-path)
                                       (html:script :src js-path))
                                     (load-js-list))
                                (map (lambda (js-mod)
                                       (html:script :type "module" :src js-mod))
                                     (js-main-module-absolute-paths)))))))))

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
                 (case cont-opcode
                   ((1)
                    (text-handler (u8vector->string (apply u8vector-append (reverse cont-frames)))))
                   ((2)
                    (binary-handler (apply u8vector-append (reverse cont-frames)))))))
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
   (output-buffer-storage :init-form (make-u8vector (* 1024 1024)))
   (command-buffering? :init-value #f)))

(add-hook! task-end-hook (lambda ()
                           (flush-commands)))

(define-application-context-slot send-context #f)

(define current-send-context (make-parameter #f))

(define (with-send-context proc)
  (cond
    ((current-send-context)
     (proc (current-send-context)))
    (else
     (application-context-slot-atomic-ref 'send-context
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

(define-application-context-slot future-table (make-hash-table 'equal?) (make-id-generator #xffffffff))

(define (with-future-table proc)
  (application-context-slot-atomic-ref 'future-table proc))

(define (allocate-future-id future)
  (with-future-table
    (lambda (tbl future-next-id)
      (define (loop)
        (let1 id (future-next-id)
          (cond
            ;; If the counter goes around the cycle, the ID can be conflict. So we need to skip IDs in the table.
            ((hash-table-contains? tbl id)
             (loop))
            (else
             (hash-table-put! tbl id future)
             id))))
      (loop))))

(define json-command-table (make-hash-table 'equal?))

(define-syntax define-action
  (syntax-rules ()
    ((_ action-name args body ...)
     (hash-table-put! json-command-table action-name (lambda args body ...)))))

(define-action "notifyException" (err)
  (raise (condition (&message (message err)) (&error))))

(define-action "startApplication" ()
  (when *initial-thunk*
    (submit-task (main-task-queue)
      (lambda ()
        (run-hook *init-hook*)
        (*initial-thunk*)))))

(define (decode-received-binary-data data)
  (call-with-input-string (u8vector->string data)
    (lambda (in)
      (let* ((future-id (read-u32 in 'little-endian))
             (json-len (read-u32 in 'little-endian))
             (json-str (u8vector->string (read-uvector <u8vector> json-len in)))
             (vals (parse-json-string json-str)))
        (while (read-u8 in) (.$ not eof-object?)
               => i
               (let* ((binary-data-len (read-u32 in 'little-endian))
                      (binary-data (read-uvector <u8vector> binary-data-len in)))
                 (vector-set! vals i binary-data)))
        (values future-id (vector->list vals))))))

(define (notify-values future-id vals)
  (with-future-table
    (lambda (tbl future-next-id)
      (let1 future (begin0
                     (hash-table-get tbl future-id #f)
                     (hash-table-delete! tbl future-id))
        (cond
          (future
           (set-future-values! future vals))
          (else
           (errorf "[BUG] Invalid future ID: ~a" future-id)))))))

(define app-close-hook (make-hook))

(define (start-websocket-dispatcher! sock in out)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (atomic-update! *num-dispatcher* (^x (+ x 1)))
          (let* ((sel (make <selector>))
                 (ctx (make <websocket-server-context>))
                 (app-context (make-application-context)))
            (define (close-websocket status data)
              (log-debug "WebSocket closed: code=~a, data=(~a)" status data)
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
              (receive (future-id vals) (decode-received-binary-data data)
                (notify-values future-id vals)))

            (parameterize ((json-special-handler (lambda (sym)
                                                   (case sym
                                                     ((false) #f)
                                                     ((true) #t)
                                                     (else sym))))
                           (application-context app-context))
              (let1 send-context (make <send-context> :websocket-output-port out)
                (slot-set! send-context 'buffer-output-port
                           (open-output-uvector (slot-ref send-context 'output-buffer-storage) :extendable #t))
                (application-context-slot-set! 'send-context send-context))

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

              (run-hook app-close-hook)

              (log-debug "WebSocket dispatcher finished")
              (close-input-port in)
              (close-output-port out)
              (connection-close sock)
              (connection-shutdown sock 'both)
              (atomic-update! *num-dispatcher* (lambda (x)
                                                 (let1 num (- x 1)
                                                   (when (and (= num 0) (client-close-on-exit?))
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

(define *jsargtype-serializer-table* (make-hash-table 'eq?))
(define *jsargtype-datastream-method-table* (make-hash-table 'eq?))

(define-syntax define-jsargtype
  (syntax-rules ()
    ((_ (type-name rest ...) datastream-method serializer)
     (begin
       (define-jsargtype type-name datastream-method serializer)
       (define-jsargtype (rest ...) datastream-method serializer-body)))
    ((_ type-name datastream-method serializer)
     (begin
       (hash-table-put! *jsargtype-serializer-table* 'type-name serializer)
       (hash-table-put! *jsargtype-datastream-method-table* 'type-name 'datastream-method)))))

(define-jsargtype f32 getFloat32 (lambda (v out)
                                   (write-f32 v out 'little-endian)))
(define-jsargtype f64 getFloat64 (lambda (v out)
                                   (write-f64 v out 'little-endian)))
(define-jsargtype s8 getInt8 (lambda (v out)
                               (write-s8 v out 'little-endian)))
(define-jsargtype s16 getInt16 (lambda (v out)
                                 (write-s16 v out 'little-endian)))
(define-jsargtype s32 getInt32 (lambda (v out)
                                 (write-s32 v out 'little-endian)))
(define-jsargtype u8 getUint8 (lambda (v out)
                                (write-u8 v out 'little-endian)))
(define-jsargtype u16 getUint16 (lambda (v out)
                                  (write-u16 v out 'little-endian)))
(define-jsargtype u32 getUint32 (lambda (v out)
                                  (write-u32 v out 'little-endian)))
(define-jsargtype u8vector getUint8Array (lambda (v out)
                                           (write-u32 (u8vector-length v) out 'little-endian)
                                           (write-uvector v out)))
(define-jsargtype f64vector getFloat64Array (lambda (v out)
                                              (write-u32 (f64vector-length v) out 'little-endian)
                                              (write-uvector v out 0 -1 'little-endian)))
(define-jsargtype boolean getBoolean (lambda (v out)
                                       (write-u8 (if v 1 0) out 'little-endian)))
(define-jsargtype future getUint32 (lambda (v out)
                                     (write-u32 (allocate-future-id v) out 'little-endian)))
(define-jsargtype object getObjectRefValue (lambda (v out)
                                             (write-u32 (jsobject-id v) out 'little-endian)))
(define-jsargtype object* getObjectRef (lambda (v out)
                                         (write-u32 (jsobject-id v) out 'little-endian)))
(define-jsargtype string getString (lambda (v out)
                                     (let1 data (ces-convert-to <u8vector> v 'utf-8)
                                       (write-u32 (u8vector-length data) out 'little-endian)
                                       (write-uvector data out))))
(define-jsargtype json getJson (lambda (v out)
                                 ;; Make a vector to make JSON to satisfy RFC4627.
                                 (let1 data (ces-convert-to <u8vector> (construct-json-string (vector v)) 'utf-8)
                                   (write-u32 (u8vector-length data) out 'little-endian)
                                   (write-uvector data out))))


(define (call-command command-id types args)
  (with-send-context
    (lambda (ctx)
      (let1 out (slot-ref ctx 'buffer-output-port)
        (write-u16 command-id out 'little-endian)
        (for-each (lambda (type arg)
                    (or (and-let1 serialize (hash-table-get *jsargtype-serializer-table* type #f)
                          (serialize arg out)
                          #t)
                        (and (hash-table-contains? *enum-table* type)
                             (write-u8 (enum-value type arg) out 'little-endian))
                        (errorf "Invalid jsarg type: ~a" type)))
                  types args))
      (unless (command-buffering?)
        (flush-commands)))))

(define (flush-commands)
  (with-send-context
    (lambda (ctx)
      (let1 output-data (get-output-uvector (slot-ref ctx 'buffer-output-port) :shared #t)
        (unless (= (uvector-length output-data) 0)
          (send-binary-frame (slot-ref ctx 'websocket-output-port) output-data)))
      (slot-set! ctx 'buffer-output-port (open-output-uvector (slot-ref ctx 'output-buffer-storage))))))

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

(define binary-command-next-id (make-id-generator #xffffffff))

(define *enum-table* (make-hash-table))

(define-class <js-procedure> ()
  ((types :init-keyword :types)
   (command-id :init-keyword :command-id)))

(define (compile-jslet arg-specs body)
  (let* ((jsmodule-name (js-vm-current-main-module))
         (var-type-list (map parse-arg-spec arg-specs))
         (command-id (binary-command-next-id))
         (name (gensym))
         (ds (gensym)))
    (register-js-stmt! jsmodule-name
                       `(begin
                          (define (,name ,ds)
                            (let ,(map (match-lambda
                                         ((var type _)
                                          `(,var ,(or (and-let1 method (hash-table-get *jsargtype-datastream-method-table* type #f)
                                                        `((~ ,ds ',method)))
                                                      (and (hash-table-contains? *enum-table* type)
                                                           `((~ ,ds 'getEnum) ,(symbol->string type)))
                                                      (errorf "Invalid jsarg type: ~a" type)))))
                                       var-type-list)
                              ,@body))
                          (Graviton.registerBinaryCommand ,command-id ,name)))
    (make <js-procedure> :command-id command-id :types (map (cut list-ref <> 1) var-type-list))))

(define-jsise-macro raise
  ((err)
   `(Graviton.notifyException ((ref ,err toString)))))

(define (jscall js-proc :rest args)
  (let1 future (make <graviton-future>)
    (call-command (slot-ref js-proc 'command-id)
                  (slot-ref js-proc 'types)
                  (cons future args))
    future))

(define (jsrun js-proc :rest args)
  (call-command (slot-ref js-proc 'command-id)
                (slot-ref js-proc 'types)
                args))

(define-macro (jslet var-spec :rest body)
  (let1 jsrun jsrun
    `(,jsrun ,(compile-jslet var-spec body)
             ,@(map (lambda (spec)
                      (list-ref (parse-arg-spec spec) 2))
                    var-spec))))

(define-macro (jslet/result var-spec :rest body)
  (let1 jscall jscall
    `(,jscall ,(compile-jslet (cons '%future-id::future var-spec) body)
              ,@(map (lambda (spec)
                       (list-ref (parse-arg-spec spec) 2))
                     var-spec))))

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

(define-record-type <id-range>
  make-id-range id-range?
  (start id-range-start set-id-range-start!)
  (end id-range-end))

(define (make-free-id-list id-size)
  (cons #t
        (cons (make-id-range 1 id-size)
              #f)))

(define (allocate-id! free-id-list)
  (let1 cell (cdr free-id-list)
    (cond
      (cell
       (let* ((id-range (car cell))
              (s (id-range-start id-range)))
         (if (= (+ s 1) (id-range-end id-range))
             (set-cdr! free-id-list (cdr cell))
             (set-id-range-start! id-range (+ s 1)))
         s))
      (else
       #f))))

(define (release-id! free-id-list id)
  (let1 cell (let loop ((cell free-id-list))
               (if (or (not (cdr cell))
                       (< id (id-range-start (cadr cell))))
                   cell
                   (loop (cdr cell))))
    (set-cdr! cell (cons (make-id-range id (+ id 1)) (cdr cell)))))

(define-class <jsobject> ()
  ((%free-id-list :allocation :class
                  :init-form (atom (make-free-id-list #x100000000)))
   (id :init-value #f)))

(define-method initialize ((obj <jsobject>) initargs)
  (next-method)
  (or (and-let1 id (atomic (slot-ref obj '%free-id-list)
                     (lambda (free-id-list)
                       (allocate-id! free-id-list)))
        (slot-set! obj 'id id)
        #t)
      (error "The jsobject ID space is exhausted.")))

(define-method jsobject-id ((obj <jsobject>))
  (or (slot-ref obj 'id)
      (errorf "~s was invalidated" obj)))

(define-method release-jsobject-id! ((obj <jsobject>))
  (and-let1 id (slot-ref obj 'id)
    (slot-set! obj 'id #f)
    (atomic (slot-ref obj '%free-id-list)
      (lambda (free-id-list)
        (release-id! free-id-list id)))))

(define-method invalidate! ((obj <jsobject>))
  (jslet ((obj*::object* obj))
      (obj*.invalidate))
  (release-jsobject-id! obj))

(define-method invalidate? ((obj <jsobject>))
  (not (not (slot-ref obj 'id))))

;;;

(define-class <browser-window> (<jsobject>)
  ())

(define-application-context-slot browser-window #f)

(define (browser-window)
  (application-context-slot-atomic-update! 'browser-window
    (lambda (win)
      (unless win
        (set! win (make <browser-window>))
        (jslet ((win*::object* win))
            (set! win*.value window)))
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
        (current-buffering-mode (command-buffering?)))
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

(define *graviton-access-log-drain* #f)
(define *graviton-error-log-drain* #t)

(define *control-channel* (make-server-control-channel))

(define (server-url sock)
  (let1 host (cond
               ((equal? (graviton-config 'wsl) "2")
                ;; WSL2 localhost can't be seen from Windows rarely. Try to look up the IP address until it is fixed.
                (if-let1 m (#/inet ((\d+)\.(\d+)\.(\d+)\.(\d+))/ (guard (e (else ""))
                                                                   (process-output->string "ip address show dev eth0")))
                         (m 1)
                         "localhost"))
               (else
                "localhost"))
    (format "http://~a:~a/" host (sockaddr-port (socket-address sock)))))

(define (invoke-player sock)
  (let* ((config-file (generate-player-config-file (server-url sock)))
         (player-path (graviton-config 'graviton-player-path)))
    (cond
      ((file-exists? player-path)
       (run-process `(,player-path "--config" ,config-file)
                    :output (player-stdout-filename)
                    :error (player-stderr-filename)))
      (else
       (run-process `("npx" "electron" "." "--config" ,config-file)
                    :directory "./player/src"
                    :output (player-stdout-filename)
                    :error (player-stderr-filename))))))

(define (generate-player-config-file url)
  (let* ((win-size (player-window-size))
         (config `((width . ,(list-ref win-size 0))
                   (height . ,(list-ref win-size 1))
                   (url . ,url)
                   (background-color . ,(client-background-color))
                   (open-dev-tools . ,(player-open-dev-tools?))))
         (config-file (receive (out filename) (sys-mkstemp (build-path (temporary-directory) "grvcfg"))
                        (construct-json config out)
                        (close-port out)
                        (if (absolute-path? filename)
                            filename
                            (simplify-path (build-path (current-directory) filename))))))
    (if (graviton-config 'wsl)
        (process-output->string `("wslpath" "-w" ,config-file))
        config-file)))

(define-class <client-config> ()
  ((port :init-value 0
         :init-keyword :port)
   (title :init-value (if (and (list? (command-line))
                               (<= (length (command-line)) 1))
                          (path-sans-extension (sys-basename (list-ref (command-line) 0)))
                          "Graviton"))
   (background-color :init-value "#FFF")))

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

(define-class <player-config> (<client-config>)
  ((type :init-value 'player)
   (window-size :init-value '(800 600))
   (open-dev-tools? :init-value #f)
   (close-on-exit? :init-value #t)
   (stdout-filename :init-form (build-path (temporary-directory) (format "graviton-player.~a.out" (sys-getpid))))
   (stderr-filename :init-form (build-path (temporary-directory) (format "graviton-player.~a.err" (sys-getpid))))))

(define-class <browser-config> (<client-config>)
  ((type :init-value 'browser)
   (port :init-value 8080
         :init-keyword :port)
   (close-on-exit? :init-value #f)))

(define *client-config* (make <browser-config> :port 8080))

(define (client-type)
  (slot-ref *client-config* 'type))

(define (client-port)
  (slot-ref *client-config* 'port))

(define (client-title)
  (slot-ref *client-config* 'title))

(define (client-background-color)
  (slot-ref *client-config* 'background-color))

(define (client-close-on-exit?)
  (slot-ref *client-config* 'close-on-exit?))

(define (player-window-size)
  (slot-ref *client-config* 'window-size))

(define (player-open-dev-tools?)
  (slot-ref *client-config* 'open-dev-tools?))

(define (player-stdout-filename)
  (slot-ref *client-config* 'stdout-filename))

(define (player-stderr-filename)
  (slot-ref *client-config* 'stderr-filename))

(define (grv-player :key
                    window-size
                    open-dev-tools?
                    title
                    background-color
                    stdout-filename
                    stderr-filename)
  (set! *client-config* (config-with-params <player-config>
                          window-size
                          open-dev-tools?
                          title
                          background-color
                          stdout-filename
                          stderr-filename)))

(define (grv-browser :key
                     port
                     title
                     background-color)
  (set! *client-config* (config-with-params <browser-config> port title background-color)))

(define-class <log-config> ()
  ((access-log-drain :init-value #f)
   (error-log-drain :init-value #t)
   (log-level :init-value 1)))

(define *log-config* (make <log-config>))

(define (grv-log-config :key
                        access-log-drain
                        error-log-drain
                        log-level)
  (set! *log-config* (config-with-params <log-config> access-log-drain error-log-drain log-level)))

(define (access-log-drain)
  (slot-ref *log-config* 'access-log-drain))

(define (error-log-drain)
  (slot-ref *log-config* 'error-log-drain))

(define (log-level)
  (slot-ref *log-config* 'log-level))

(define (grv-main thunk)
  (set! *initial-thunk* thunk)
  (let ((port (client-port)))
    (start-http-server :port port
                       :access-log (access-log-drain)
                       :error-log (error-log-drain)
                       :control-channel *control-channel*
                       :startup-callback (lambda (socks)
                                           (run-scheduler)
                                           (case (client-type)
                                             ((player)
                                              (invoke-player (car socks)))
                                             ((browser)
                                              (log-info "Graviton server is running at")
                                              (log-info "~a" (server-url (first socks))))))
                       :shutdown-callback (lambda ()
                                            (shutdown-scheduler!)))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_ expr ...)
     (grv-main (lambda () expr ...)))))
