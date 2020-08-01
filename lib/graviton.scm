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
  (use graviton.jsbridge)
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

  (export grv-start
          grv-begin
          grv-exit

          grv-player
          grv-browser
          grv-log-config

          current-pool
          task-queue-worker-timeout
          task-queue-worker-timeout-set!

          async-apply
          async

          with-jstransaction

          client-window
          client-window-size
          client-close

          app-start-hook
          app-close-hook

          current-task-queue
          async-task-queue
          default-async-task-queue

          define-action

          define-url-path

          resource-url

          add-jsevent-listener!
          remove-jsevent-listener!

          <jsevent>
          fire-event
          next-event
          all-events
          event-stream-closed?
          event-stream-close
          capture-jsevent
          set-frame-interval!
          frame-sync))

(select-module graviton)

;;;

(log-open #t)

;;;

(define *url-path-map-alist* '())

(define (register-url-path! url-path file-path)
  (set! *url-path-map-alist* (append *url-path-map-alist*
                                    (list
                                      (cons (simplify-path (string-append "/" url-path "/"))
                                            file-path)))))

(define-syntax define-url-path
  (syntax-rules ()
    ((_ url-path file-path)
     (register-url-path! url-path file-path))))

(define-url-path "graviton" (graviton-js-directory))

(define (url->file-path path)
  (let1 path (simplify-path path)
    (let loop ((path-map-alist *url-path-map-alist*))
      (match path-map-alist
        (()
         #f)
        (((url-path . file-path) rest ...)
         (or (and-let1 path (and (string-prefix? url-path path)
                                 (build-path file-path (string-drop path (string-length url-path))))
               (and (file-is-readable? path)
                    path))
             (loop rest)))))))

(define-http-handler #/(\/.*\/.*)/
  (lambda (req app)
    (let1 url-path (slot-ref req 'path)
      (cond
        ((url->file-path url-path)
         => (lambda (file-path)
              (respond/ok req `(file ,file-path) :content-type (estimate-content-type url-path))))
        (else
         (respond/ng req 404))))))

(define-http-handler #/\/[^\/]+.m?js/
  (lambda (req app)
    (let1 url-path (slot-ref req 'path)
      (cond
        ((and (equal? (sys-dirname url-path) "/")
              (get-js-code (sys-basename url-path)))
         => (lambda (js-code)
              (respond/ok req js-code :content-type (estimate-content-type url-path))))
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

(define-action "notifyException" (err stacktrace)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "*** ~a\n" err)
      (display "Stack Trace in client:\n")
      (display "_______________________________________\n")
      (display stacktrace)
      (newline)))
  (app-exit 70))

(define-action "startApplication" ()
  (when *initial-thunk*
    (run-hook *init-hook*)
    (let1 exit-code (*initial-thunk*)
      (app-exit (if (integer? exit-code) exit-code 70)))))

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
  (delay (vector->list
           (force
             (jslet/result ()
               (result (vector window.innerWidth window.innerHeight)))))))

(define (client-close)
  (jslet ()
    (Graviton.closeConnection)))

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

(define (grv-browser :key port title background-color)
  (set! *client-config* (config-with-params <browser-config> port title background-color)))

(define (grv-start thunk)
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
     (grv-start (lambda () expr ...)))))

(define (grv-exit code)
  (app-exit code)
  (task-quit))
