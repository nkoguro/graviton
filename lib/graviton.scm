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
  (use graviton.app)
  (use graviton.async)
  (use graviton.comm)
  (use graviton.config)
  (use graviton.jsise)
  (use graviton.misc)
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
          task-queue-min-num-threads
          task-queue-min-num-threads-set!
          task-queue-max-num-threads
          task-queue-max-num-threads-set!
          task-queue-worker-timeout
          task-queue-worker-timeout-set!

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

          client-window
          client-window-size
          client-close

          app-start-hook
          app-close-hook
          app-exit

          current-task-queue
          async-task-queue
          default-async-task-queue

          define-action

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

(log-open #t)

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


(define *initial-thunk* #f)
(define *init-hook* (make-hook))

(add-hook! task-end-hook (lambda ()
                           (flush-commands)))

(define-action "notifyException" (err)
  (raise (condition (&message (message err)) (&error))))

(define-action "startApplication" ()
  (when *initial-thunk*
    (run-hook *init-hook*)
    (*initial-thunk*)))

(define (start-websocket-dispatcher! sock in out)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (let1 exit-code (websocket-main-loop in out)
            (log-debug "WebSocket dispatcher finished: ~a" exit-code)
            (close-input-port in)
            (close-output-port out)
            (connection-close sock)
            (connection-shutdown sock 'both)
            (when (client-close-on-exit?)
              (terminate-server-loop *control-channel* exit-code))))))))

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

;; TODO: Deprecate send-context
(define-class <send-context> ()
  ((buffer-output-port :init-keyword :buffer-output-port)
   (output-buffer-storage :init-form (make-u8vector (* 1024 1024)))
   (command-buffering? :init-value #f)))

(define-application-context-slot send-context #f)

(define current-send-context (make-parameter #f))

(define (init-send-context)
  (let1 send-context (make <send-context>)
    (slot-set! send-context
               'buffer-output-port
               (open-output-uvector (slot-ref send-context 'output-buffer-storage) :extendable #t))
    (application-context-slot-set! 'send-context send-context)))

(add-hook! app-start-hook init-send-context)

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

(define (flush-commands)
  (let1 output-data (with-send-context
                      (lambda (ctx)
                        (begin0
                          (get-output-uvector (slot-ref ctx 'buffer-output-port) :shared #t)
                          (slot-set! ctx 'buffer-output-port (open-output-uvector (slot-ref ctx 'output-buffer-storage))))))
    (unless (= (uvector-length output-data) 0)
      (send-binary-frame (application-context-slot-ref 'websocket-output-port) output-data))))

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

(define (write-command-args types args out)
  (for-each (lambda (type arg)
              (or (and-let1 serialize (hash-table-get *jsargtype-serializer-table* type #f)
                    (serialize arg out)
                    #t)
                  (and (hash-table-contains? *enum-table* type)
                       (write-u8 (enum-value type arg) out 'little-endian))
                  (errorf "Invalid jsarg type: ~a" type)))
            types args))


(define (call-command command-id types args)
  (with-send-context
    (lambda (ctx)
      (let1 out (slot-ref ctx 'buffer-output-port)
        (write-u16 command-id out 'little-endian)
        (write-command-args types args out))
      (unless (command-buffering?)
        (flush-commands)))))

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

(define-class <jsprocedure> ()
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
    (make <jsprocedure> :command-id command-id :types (map (cut list-ref <> 1) var-type-list))))

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

(define-class <jsenum> ()
  ((symbol->value-table :init-form (make-hash-table))
   (jsvalue->symbol-table :init-form (make-hash-table 'equal?))))

(define (make-enum jsmodule-name enum-name symbol->jsvalue-list)
  (let ((enum (make <jsenum>))
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

(define-class <client-window> (<jsobject>)
  ())

(define-application-context-slot client-window #f)

(define (client-window)
  (application-context-slot-atomic-update! 'client-window
    (lambda (win)
      (unless win
        (set! win (make <client-window>))
        (jslet ((win*::object* win))
            (set! win*.value window)))
      win)))

(define (client-window-size)
  (jslet/result ()
    (result window.innerWidth window.innerHeight)))

(define (client-close)
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
