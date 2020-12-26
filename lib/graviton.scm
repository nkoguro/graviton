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
  (use graviton.event)
  (use graviton.jsffi)
  (use graviton.misc)
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

  (extend graviton.async graviton.browser-objects graviton.event graviton.canvas graviton.audio graviton.text)

  (export grv-start
          grv-started?
          grv-begin
          grv-exit

          grv-player
          grv-browser
          grv-log-config

          with-jstransaction

          bind-url-path

          define-document-content

          make-jsobject
          ))

(select-module graviton)

;;;

(log-open #t)

;;;

(define *load-css-list* '())

(define (load-css css-path)
  (push! *load-css-list* css-path))

(define (load-css-list)
  (reverse *load-css-list*))

;;;

(define *global-url-handler-alist* '())

(define-application-context-slot url-handler-alist '())

(define-method bind-url-path ((url-path <string>) (file-path <string>))
  (let1 canonical-url-path (simplify-path (if (file-is-directory? file-path)
                                            (string-append "/" url-path "/")
                                            (string-append "/" url-path)))
    (cond
      ((application-context)
       (application-context-slot-atomic-update! 'url-handler-alist
         (lambda (alist)
           (assoc-set! alist canonical-url-path file-path))))
      (else
       (set! *global-url-handler-alist* (assoc-set! *global-url-handler-alist* canonical-url-path file-path))))))

(define-method bind-url-path ((url-path <string>) (proc <procedure>))
  (cond
    ((application-context)
     (application-context-slot-atomic-update! 'url-handler-alist
       (lambda (alist)
         (assoc-set! alist (simplify-path url-path) proc))))
    (else
     (set! *global-url-handler-alist* (assoc-set! alist (simplify-path url-path) proc)))))

(bind-url-path "_g" (graviton-js-directory))

(define (url-path->body&content-type ctx url-path)
  (let1 url-path (simplify-path url-path)
    (let loop ((url-handler-alist (append (if ctx
                                            (parameterize ((application-context ctx))
                                              (application-context-slot-ref 'url-handler-alist))
                                            '())
                                          *global-url-handler-alist*)))
      (match url-handler-alist
        (()
         (values #f #f))
        (((target-url-path . (? string? file-path)) rest ...)
         (let1 path (and (string-prefix? target-url-path url-path)
                         (build-path file-path (string-drop url-path (string-length target-url-path))))
           (if (and path (file-is-readable? path))
             (values `(file ,path) (estimate-content-type path))
             (loop rest))))
        (((target-url-path . (? procedure? proc)) rest ...)
         (if (equal? target-url-path url-path)
           (proc)
           (loop rest)))))))

(define-http-handler #/\/_m\/.*\.m?js/
  (lambda (req app)
    (let1 url-path (slot-ref req 'path)
      (or (and-let1 js-code (get-js-code url-path)
            (respond/ok req js-code :content-type "text/javascript"))
          (respond/ng req 404)))))

;;;

(load-css "/_g/graviton.css")

(define document-body-proc
  (lambda ()
    (html:body)))

(define-syntax define-document-content
  (syntax-rules ()
    ((_ body)
     (set! document-body-proc
           (lambda ()
             body)))))

(define-constant HEADER-APPLICATION-CONTEXT-ID "id")

(define-http-handler "/"
  (lambda (req app)
    (guard (e (else (report-error e)
                    (exit 70)))
      (let1 ctx (make-application-context)
        (response-cookie-add! req HEADER-APPLICATION-CONTEXT-ID (~ ctx'id)))
      (respond/ok req (tree->string
                        (html:html
                         (html:head
                          (html:meta :charset "UTF-8")
                          (html:link :rel "icon" :href "data:png;base64,")
                          (map (lambda (css)
                                 (html:link :rel "stylesheet" :type "text/css" :href css))
                               (load-css-list))
                          (append
                            (map (lambda (js-path)
                                   (html:script :src js-path))
                                 (load-js-list))
                            (map (lambda (js-mod)
                                   (html:script :type "module" :src js-mod))
                                 (js-main-module-absolute-paths)))
                          (html:title (client-title)))
                         (document-body-proc)))))))

(define *init-hook* (make-hook))

(define-action "notifyException" (err stacktrace)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "*** ~a\n" err)
      (display "Stack Trace in client:\n")
      (display "_______________________________________\n")
      (display stacktrace)
      (newline)))
  (app-exit 70))

(define (start-websocket-dispatcher! ctx sock in out)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (let1 exit-code (websocket-main-loop ctx in out)
            (log-debug "WebSocket dispatcher finished: ~a" exit-code)
            (close-input-port in)
            (close-output-port out)
            (connection-close sock)
            (connection-shutdown sock 'both)
            (when (eq? (client-type) 'player)
              (terminate-server-loop *control-channel* exit-code))))))))

(define-http-handler "/_s"
  (lambda (req app)
    ;; Set TCP_NODELAY to reduce latency if possible.
    (when (and (global-variable-bound? (current-module) 'IPPROTO_TCP)
               (global-variable-bound? (current-module) 'TCP_NODELAY))
      (socket-setsockopt (request-socket req) IPPROTO_TCP TCP_NODELAY 1))
    (let ((in (request-iport req))
          (out (request-oport req)))
      (let-params req ((upgrade "h")
                       (connection "h")
                       (sec-websocket-key "h")
                       (sec-websocket-version "h")
                       (ctx "c:id" :convert lookup-application-context))
                  (cond
                    ((and (string-contains-ci upgrade "websocket")
                          (string-contains-ci connection "upgrade")
                          sec-websocket-key
                          (equal? sec-websocket-version "13")
                          ctx)
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
                     (start-websocket-dispatcher! ctx (request-socket req) in out)
                     req)
                    (else
                     (respond/ng req 400)))))))

(define-http-handler #/.+/
  (lambda (req app)
    (let* ((url-path (slot-ref req 'path))
           (ctx (lookup-application-context (request-cookie-ref req HEADER-APPLICATION-CONTEXT-ID))))
      (receive (body content-type) (url-path->body&content-type ctx url-path)
        (if body
          (respond/ok req body :content-type content-type)
          (respond/ng req 404))))))

;;;


(define-method make-jsobject ((jsobject-class <jsobject-meta>) :rest args)
  (apply make-jsobject (~ jsobject-class'jsclass) args))

(define-method make-jsobject ((jsclass <string>) :rest args)
  (jslet/result ((jsclass::string)
                 (args (list->vector args)))
    (result (Graviton.makeJSObject jsclass args))))

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
                   (open-dev-tools . ,(player-open-dev-tools?))
                   (show . ,(player-show?))
                   (resizable . ,(player-resizable?))))
         (config-file (receive (out filename) (sys-mkstemp (build-path (temporary-directory) "grvcfg"))
                        (construct-json config out)
                        (close-port out)
                        (if (absolute-path? filename)
                          filename
                          (simplify-path (build-path (current-directory) filename))))))
    (if (graviton-config 'wsl)
      (process-output->string `("wslpath" "-w" ,config-file))
      config-file)))

(define-class <player-config> (<client-config>)
  ((type :init-value 'player)
   (window-size :init-value '(800 600))
   (open-dev-tools? :init-value #f)
   (close-on-exit? :init-value #t)
   (show? :init-value #t)
   (resizable? :init-value #f)
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

(define (client-close-on-exit?)
  (slot-ref *client-config* 'close-on-exit?))

(define (player-window-size)
  (slot-ref *client-config* 'window-size))

(define (player-open-dev-tools?)
  (slot-ref *client-config* 'open-dev-tools?))

(define (player-show?)
  (slot-ref *client-config* 'show?))

(define (player-resizable?)
  (slot-ref *client-config* 'resizable?))

(define (player-stdout-filename)
  (slot-ref *client-config* 'stdout-filename))

(define (player-stderr-filename)
  (slot-ref *client-config* 'stderr-filename))

(define (grv-player :key
                    window-size
                    open-dev-tools?
                    title
                    show?
                    resizable?
                    stdout-filename
                    stderr-filename)
  (set! *client-config* (config-with-params <player-config>
                          window-size
                          open-dev-tools?
                          title
                          show?
                          resizable?
                          stdout-filename
                          stderr-filename)))

(define (grv-browser :key port title)
  (set! *client-config* (config-with-params <browser-config> port title)))

(define (grv-start thunk)
  (main-worker-thunk-set! thunk)
  (let ((port (client-port)))
    (start-http-server :port port
                       :access-log (grv-access-log-drain)
                       :error-log (grv-error-log-drain)
                       :control-channel *control-channel*
                       :startup-callback (lambda (socks)
                                           (case (client-type)
                                             ((player)
                                              (invoke-player (car socks)))
                                             ((browser)
                                              (log-info "Graviton server is running at")
                                              (log-info "~a" (server-url (first socks)))))))))

(define-syntax grv-begin
  (syntax-rules ()
    ((_ expr ...)
     (grv-start (lambda () expr ...)))))

(define (grv-exit :optional (code 0))
  (app-exit code))
