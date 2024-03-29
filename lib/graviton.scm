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
  (use gauche.config)
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
  (use graviton.repl)
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
  (use www.css)

  (extend graviton.async graviton.browser-objects graviton.event graviton.canvas graviton.audio)

  (export grv-start-player
          grv-start-server

          grv-title
          grv-title-set!

          grv-log-config

          autoload-css
          bind-url-path
          file->url
          data->url
          json->url
          sxml->url

          flush-client-request

          grv-window
          let-elements
          import-elements

          <window-parameter>
          make-window-parameter
          make-window-parameter*
          window-parameter-atomic-ref
          window-parameter-atomic-update!

          query-parameters

          define-global-jsobject

          let-jsproperties

          user-agent

          grv-config
          grv-config-parameter
          client-is-player?
          client-is-browser?

          with-window
          open-window
          close-window

          ;; From jsffi
          import-js
          define-jsvar
          define-jsfn
          jslet
          jslet/async
          jslet/await

          make-repl-group
          grv-repl
          next-repl
          list-repl
          select-repl
          ))

(select-module graviton)

;;;

(log-open #t)

;;;

;; module -> (css filename ...)
(define *autoload-css-table* (make-hash-table))

(define (add-autoload-css! module :rest css-paths)
  (for-each (lambda (path)
              (hash-table-push! *autoload-css-table* module path))
            css-paths))

(define-syntax autoload-css
  (syntax-rules ()
    ((_ css-path ...)
     (add-autoload-css! (current-module) css-path ...))))

(define (%autoload-css-list cache module)
  (cond
    ((hash-table-get cache module #f)
     => values)
    (else
     (rlet1 css-list (let1 css-list (append (apply append (map (cut %autoload-css-list cache <>) (module-imports module)))
                                            (apply append (map (cut %autoload-css-list cache <>) (module-parents module)))
                                            (reverse (hash-table-get *autoload-css-table* module '())))
                       ;; Remove duplicated css
                       ;; Keep later element (e.g. (A B C E D E) -> (A B C D E))
                       (let1 tbl (make-hash-table 'equal?)
                         (reverse (fold (lambda (path css-list)
                                          (cond
                                            ((hash-table-contains? tbl path)
                                             css-list)
                                            (else
                                             (hash-table-put! tbl path #t)
                                             (cons path css-list))))
                                        '()
                                        (reverse css-list)))))
       (hash-table-put! cache module css-list)))))

(define (autoload-css-list module)
  (%autoload-css-list (make-hash-table) module))

;;;

(define *grv-default-title*
  (if (and (list? (command-line))
           (<= 1 (length (command-line))))
    (path-sans-extension (sys-basename (list-ref (command-line) 0)))
    "Graviton"))

(define (grv-title)
  (cond
    ((window-context)
     (~ document'title))
    (else
     *grv-default-title*)))

(define (grv-title-set! title)
  (cond
    ((window-context)
     (set! (~ document'title) title))
    (else
     (set! *grv-default-title* title))))

;;;

(define *global-url-handler-alist* '())

(define-method bind-url-path ((url-path <string>) (file-path <string>))
  (let1 canonical-url-path (simplify-path (if (file-is-directory? file-path)
                                            (string-append "/" url-path "/")
                                            (string-append "/" url-path)))
    (set! *global-url-handler-alist* (assoc-set! *global-url-handler-alist* canonical-url-path file-path))))

(define-method bind-url-path ((url-path <string>) (proc <procedure>))
  (set! *global-url-handler-alist* (assoc-set! alist (simplify-path url-path) proc)))

(bind-url-path "_g" (graviton-js-directory))

(define (url-path->body&content-type url-path)
  (let1 url-path (simplify-path url-path)
    (let loop ((url-handler-alist *global-url-handler-alist*))
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
            (response-header-replace! req "Cache-Control" "private, max-age=86400")
            (respond/ok req js-code :content-type "text/javascript"))
          (respond/ng req 404)))))

(define *id->resource-body-table* (make-hash-table 'equal?))
(define *resource-body->url-table* (make-hash-table 'equal?))
(define resource-id-generator (make-id-generator))

(define-http-handler #/\/_r\/(.*)/
  (lambda (req app)
    (let1 res-id ((slot-ref req 'path-match) 1)
      (match-let1 (content-type body) (hash-table-get *id->resource-body-table* res-id '(#f #f))
        (cond
          ((and content-type body)
           (respond/ok req body :content-type content-type))
          (body
           (respond/ok req body))
          (else
           (respond/ng req 404)))))))

(define (allocate-resource content-type body)
  (let1 content-type&body (list content-type body)
    (cond
      ((hash-table-get *resource-body->url-table* content-type&body #f)
       => (lambda (url) url))
      (else
       (let* ((res-id (let loop ()
                        (let1 id (number->string (random-integer 18446744073709551616) 36)
                          (if (hash-table-contains? *id->resource-body-table* id)
                            (loop)
                            id))))
              (url (string-append "/_r/" res-id)))
         (hash-table-put! *id->resource-body-table* res-id content-type&body)
         (hash-table-put! *resource-body->url-table* content-type&body url)
         url)))))

(define (file->url file :optional (content-type (estimate-content-type file)))
  (unless content-type
    (errorf "No content-type specified: ~a" file))
  (unless (file-is-readable? file)
    (errorf "File not found: ~a" file))
  (allocate-resource content-type `(file ,file)))

(define (data->url data content-type)
  (unless content-type
    (errorf "No content-type specified: ~s" data))
  (allocate-resource content-type `(file ,file)))

(define (json->url json)
  (allocate-resource #f `(json ,json)))

(define (sxml->url sxml)
  (allocate-resource #f `(sxml ,sxml)))

;;;

(define *path->window-table* (make-hash-table 'equal?))
(define window-id-generator (make-id-generator))

(define-class <window-controller> ()
  ((id :init-form (window-id-generator))
   (path :init-keyword :path)
   (page :init-keyword :page)
   (thunk :init-keyword :thunk)
   (temporary? :init-value #f)))

(define-method initialize ((win-controller <window-controller>) initargs)
  (next-method)

  (slot-set! win-controller 'temporary? (if (slot-ref win-controller 'path)
                                          #f
                                          #t))

  (let1 path (or (slot-ref win-controller 'path)
                 (format "/_w/~a" (slot-ref win-controller 'id)))
    (hash-table-put! *path->window-table* path win-controller)
    (slot-set! win-controller 'path path)))

(define (make-window-controller path page thunk)
  (make <window-controller> :path path :page page :thunk thunk))

(define-class <grv-window> ()
  ((page :init-keyword :page)
   (width :init-keyword :width)
   (height :init-keyword :height)
   (resizable? :init-keyword :resizable?)
   (show? :init-keyword :show?)))

(define (make-window module page-form)
  (let loop ((page-form page-form)
             (title *grv-default-title*)
             (css-list (autoload-css-list module))
             (js-list '())
             (head '())
             (body (html:body))
             (width 800)
             (height 600)
             (resizable? #t)
             (show? #t))
    (match page-form
      (()
       (make <grv-window>
         :page (tree->string
                 (html:html
                  (html:head
                   (html:meta :charset "UTF-8")
                   (map (lambda (css)
                          (html:link :rel "stylesheet" :type "text/css" :href css))
                        css-list)
                   (map (match-lambda
                          ((js-path . type)
                           (html:script :type type :src js-path)))
                        (load-js+type-list))
                   (map (lambda (js-mod)
                          (html:script :type "module" :src (string-append js-mod "?" (base64-encode-string (sha1-digest-string (get-js-code js-mod))))))
                        (js-main-module-absolute-paths))
                   (map (lambda (js)
                          (html:script :src js))
                        js-list)
                   (html:title title)
                   head)
                  body))
         :width width
         :height height
         :resizable? resizable?
         :show? show?))
      ((':title title rest ...)
       (loop rest
             (or title *grv-default-title*)
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':css (? string? css) rest ...)
       (loop rest
             title
             (append css-list (list css))
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':css sxcss rest ...)
       (let1 css (allocate-resource "text/css" (call-with-output-string (cut construct-css sxcss <>)))
         (loop rest
               title
               (append css-list (list css))
               js-list
               head
               body
               width
               height
               resizable?
               show?)))
      ((':js (? string? js) rest ...)
       (loop rest
             title
             css-list
             (append js-list (list js))
             head
             body
             width
             height
             resizable?
             show?))
      ((':head additional-head rest ...)
       (loop rest
             title
             css-list
             js-list
             (append head (list additional-head))
             body
             width
             height
             resizable?
             show?))
      ((':body body rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':width width rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':height height rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':resizable? resizable? rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      ((':show? show? rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body
             width
             height
             resizable?
             show?))
      (_
       (errorf "Invalid page form: ~s" page-form)))))

(define-syntax grv-window
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ page-form ...)
         (quasirename rename
           `(make-window (current-module) (list ,@page-form))))
        (_
         (errorf "malformed grv-window: ~s" form))))))

(define (lookup-window-controller path)
  (hash-table-get *path->window-table* (simplify-path (string-append "/" path)) #f))

(define (delete-window-controller path)
  (hash-table-delete! *path->window-table* path))

;;;

(define (%find-element id eid)
  (let1 alist (window-context-slot-ref (window-context) 'window-element-alist)
    (or (assoc-ref alist id #f)
        (let1 element (document'get-element-by-id eid)
          (cond
            ((eq? element 'null)
             #f)
            (else
             (window-context-slot-set! (window-context)
                                       'window-element-alist
                                       (acons id
                                              element
                                              (window-context-slot-ref (window-context) 'window-element-alist)))
             element))))))

(define-syntax let-elements
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ () body ...)
         (quasirename rename
           `(let () ,@body)))
        ((_ ((? symbol? id) rest ...) body ...)
         (quasirename rename
           `(let1 ,id (%find-element ',id ,(symbol->string id))
              (let-elements ,rest ,@body))))
        ((_ (((? symbol? var) name) rest ...) body ...)
         (quasirename rename
           `(let1 ,var (%find-element ',var ,name)
              (let-elements ,rest ,@body))))
        (_
         (errorf "malformed let-elements: ~s" form))))))

(define-syntax import-elements
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_)
         (quasirename rename
           `(begin)))
        ((_ (? symbol? id) rest ...)
         (quasirename rename
           `(begin
              (define ,id (%find-element ',id ,(symbol->string id)))
              (import-elements ,@rest))))
        ((_ ((? symbol? var) name) rest ...)
         (quasirename rename
           `(begin
              (define ,var (%find-element ',var ,name))
              (import-elements ,@rest))))
        (_
         (errorf "malformed import-elements: ~s" form))))))

;;;

(autoload-css "/_g/graviton.css")

(define-action "notifyException" (err stacktrace)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "*** ~a\n" err)
      (display "Stack Trace in client:\n")
      (display "_______________________________________\n")
      (display stacktrace)
      (newline)))
  (app-exit 70))

(define *shutdown-if-all-connection-closed?* #f)
(define *num-connection-atom* (atom 0))
(define *requested?-atom* (atom #f))

(define (start-websocket-dispatcher! ctx sock in out req-user-agent thunk)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (atomic-update! *num-connection-atom* (^x (+ x 1)))
          (atomic-update! *requested?-atom* (^_ #t))
          (when (client-is-browser?)
            (log-info "Connection established: ~s" ctx))
          (let1 exit-code (websocket-main-loop ctx in out req-user-agent thunk)
            (log-framework-debug "WebSocket dispatcher finished: ~a" exit-code)
            (when (client-is-browser?)
              (log-info "Connection closed: ~s (exit-code: ~a)" ctx exit-code))
            (close-input-port in)
            (close-output-port out)
            (connection-close sock)
            (connection-shutdown sock 'both)
            (atomic-update! *num-connection-atom* (^x (- x 1)))
            (when (and *shutdown-if-all-connection-closed?*
                       (atomic *num-connection-atom* (^x (= x 0))))
              (terminate-server-loop *control-channel* exit-code)))))
      (format "websocket-main-loop: ~s" ctx))))

(define query-parameters (make-window-parameter '()))

(define-http-handler #/\/_s(\/.*)?/
  (lambda (req app)
    ;; Set TCP_NODELAY to reduce latency if possible.
    (when (and (global-variable-bound? (current-module) 'IPPROTO_TCP)
               (global-variable-bound? (current-module) 'TCP_NODELAY))
      (socket-setsockopt (request-socket req) IPPROTO_TCP TCP_NODELAY 1))
    (guard (e (else (report-error e)))
      (let* ((in (request-iport req))
             (out (request-oport req))
             (path (or ((~ req'path-match) 1) "/"))
             (win-controller (lookup-window-controller path))
             (thunk (and win-controller (~ win-controller'thunk))))
        (let-params req ((upgrade "h")
                         (connection "h")
                         (sec-websocket-key "h")
                         (sec-websocket-version "h")
                         (req-user-agent "h:user-agent"))
          (cond
            ((not thunk)
             (respond/ng req 404))
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
             (log-framework-debug "WebSocket connected")
             (when (~ win-controller'temporary?)
               (delete-window-controller path))
             (let1 ctx (make-window-context)
               (window-parameter-set! ctx query-parameters (~ req'params))
               (start-websocket-dispatcher! ctx (request-socket req) in out req-user-agent thunk))
             req)
            (else
             (respond/ng req 400))))))))

(define-http-handler #/.+/
  (lambda (req app)
    (let1 url-path (slot-ref req 'path)
      (cond
        ((lookup-window-controller url-path)
         => (lambda (win-controller)
              (respond/ok req (~ win-controller'page))))
        (else
         (receive (body content-type) (url-path->body&content-type url-path)
           (if body
             (respond/ok req body :content-type content-type)
             (respond/ng req 404))))))))

;;;


(define-method make-jsobject ((jsobject-class <jsobject-meta>) :rest args)
  (apply make-jsobject (~ jsobject-class'jsclass) args))

(define-method make-jsobject ((jsclass <string>) :rest args)
  (jslet/await ((jsclass::string)
                (args (list->vector args)))
    (respond (Graviton.makeJSObject jsclass args))))

;;;

(define (open-browser url)
  (let ((browser (get-environment-variable "BROWSER"))
        (arch (gauche-config "--arch")))
    (run-process (cond
                   (browser
                    (list browser url))
                   ((#/mingw/i arch)
                    (list "cmd" "/c" "start" url))
                   ((#/darwin/i arch)
                    (list "open" url))
                   (else
                    (list "xdg-open" url)))
                 :redirects '((>& 2 1) (> 1 :null)))))

;;;

(define *server-started?* #f)

(define-class <grv-config> ()
  ((port :init-keyword :port)
   (host :init-keyword :host)
   (protocol :init-keyword :protocol)
   (client :init-keyword :client)
   (mode :init-keyword :mode)
   (access-log :init-keyword :access-log)
   (error-log :init-keyword :error-log)
   (iframe-window? :init-keyword :iframe-window?)))

(define *grv-config* #f)

(define (player-exists?)
  (file-is-executable? (graviton-config 'graviton-player-path)))

(define (grv-config :key
                    (host "localhost")
                    (protocol "http")
                    (client #f)
                    (mode #f)
                    (port #f)
                    (access-log #f)
                    (error-log #f)
                    (iframe-window? (eq? client 'browser)))
  (unless *server-started?*
    (let* ((mode (or mode
                     (cond
                       (client
                        (display ":client keyword parameter is deprecated. Use :mode parameter instead.\n"
                                 (current-error-port))
                        (if (eq? client 'browser)
                          'server
                          client))
                       ((player-exists?)
                        'player)
                       (else
                        'browser))))
           (client-type (if (eq? mode 'server)
                          'browser
                          mode)))
      (set! *grv-config* (make <grv-config>
                           :port (or port
                                     (case mode
                                       ((server)
                                        (graviton-config 'server-default-port))
                                       (else
                                        (graviton-config 'app-default-port))))
                           :mode mode
                           :host host
                           :protocol protocol
                           :client client-type
                           :access-log access-log
                           :error-log error-log
                           :iframe-window? iframe-window?)))))

(define (grv-config-parameter name)
  (case name
    ((port) (~ *grv-config*'port))
    ((host) (~ *grv-config*'host))
    ((protocol) (~ *grv-config*'protocol))
    ((client) (~ *grv-config*'client))
    ((mode) (~ *grv-config*'mode))
    ((access-log) (~ *grv-config*'access-log))
    ((error-log) (~ *grv-config*'error-log))
    ((iframe-window?) (~ *grv-config*'iframe-window?))
    (else
     (errorf "Invalid parameter name: ~s" name))))

(define (client-is-player?)
  (eq? (grv-config-parameter 'client) 'player))

(define (client-is-browser?)
  (eq? (grv-config-parameter 'client) 'browser))

;; Set default config.
(grv-config)

(define *graviton-access-log-drain* #f)
(define *graviton-error-log-drain* #t)

(define *control-channel* #f)

(define (server-url sock)
  (format "~a://~a:~a/"
          (grv-config-parameter 'protocol)
          (grv-config-parameter 'host)
          (sockaddr-port (socket-address sock))))

(define (invoke-player url width height show? resizable?)
  (let* ((player-args (generate-player-args url width height show? resizable?))
         (player-path (or (graviton-config 'alternative-player-path)
                          (graviton-config 'graviton-player-path)))
         (stdout-filename (build-path (temporary-directory) (format "graviton-player.~a.out" (sys-getpid))))
         (stderr-filename (build-path (temporary-directory) (format "graviton-player.~a.err" (sys-getpid)))))
    (let1 player-process (case (grv-config-parameter 'client)
                           ((player)
                            (run-process `(,player-path ,@player-args)
                                         :input :null
                                         :output stdout-filename
                                         :error stderr-filename))
                           ((dev-player)
                            (run-process `("npx" "electron" "." ,@player-args)
                                         :directory "./player/src"
                                         :input :null
                                         :output stdout-filename
                                         :error stderr-filename))
                           (else
                            (errorf "Invalid client: ~s" (grv-config-parameter 'client))))
      (thread-start! (make-thread (lambda ()
                                    (process-wait player-process)
                                    (let* ((status (process-exit-status player-process))
                                           (exit-code (sys-wait-exit-status status)))
                                      ;; If websocket connection is established, the connection handler will quit this app
                                      ;; when the connection is lost.
                                      ;; However, the player exits before such connection is established, no one can quit this app.
                                      ;; So we need to quit this app here when the player exits in the no-connection case.
                                      (when (and (not (= exit-code 0))
                                                 (not (atom-ref *requested?-atom*)))
                                        (format (current-error-port) "'~a ~a' aborted (~a)~%" player-path (string-join player-args) exit-code)
                                        (display (file->string stderr-filename) (current-error-port))
                                        (exit 70)))))))))

(define (generate-player-args url width height show? resizable?)
  `(,@(if width
        `("--width" ,width)
        '())
    ,@(if height
        `("--height" ,height)
        '())
    "--visible" ,(if show? "true" "false")
    "--resizable" ,(if resizable? "true" "false")
    ;; NOTE: It looks like graviton-player invocation will fail if some args are placed after the url on WSL. I'm not sure why it happens.
    ,url))

(define (grv-start-server shutdown-if-all-connection-closed?
                          startup-callback
                          :key
                          (access-log #f)
                          (error-log #f))
  (when *server-started?*
    (error "Graviton server is already started."))
  (set! *shutdown-if-all-connection-closed?* shutdown-if-all-connection-closed?)
  (set! *control-channel* (make-server-control-channel))
  (start-http-server :port (grv-config-parameter 'port)
                     :access-log (grv-config-parameter 'access-log)
                     :error-log (grv-config-parameter 'error-log)
                     :control-channel *control-channel*
                     :startup-callback (lambda (socks)
                                         (set! *server-started?* #t)
                                         (startup-callback (server-url (first socks))))
                     :shutdown-callback (lambda ()
                                          (set! *server-started?* #f)
                                          (set! *control-channel* #f))))

(define (grv-exit :optional (code 0))
  (flush-client-request)
  (app-exit code))

;;;

(define (open-client-window path width height resizable?)
  (unless (window-context)
    (error "window-context not found"))
  (jslet ((path::string)
          (width)
          (height)
          (resizable?)
          (use-iframe? (grv-config-parameter 'iframe-window?)))
    (Graviton.openWindow path width height resizable? use-iframe?)))

(define %close-window-message (gensym))

(define (%with-window win thunk)
  (let* ((win (or win
                  (grv-window :body (html:body) :show? #f)))
         (path (if *server-started?*
                 #f
                 "/"))
         (thunk (lambda ()
                  (add-message-handler! %close-window-message
                    (lambda ()
                      (flush-client-request)
                      (for-each worker-close (all-workers))
                      (app-exit 0)
                      (asleep 0)))
                  (thunk)))
         (win-controller (make-window-controller path (~ win'page) thunk))
         (use-player? (memq (grv-config-parameter 'client) '(player dev-player)))
         (server-mode? (eq? (grv-config-parameter 'mode) 'server))
         (width (~ win'width))
         (height (~ win'height))
         (resizable? (~ win'resizable?)))
    (cond
      (*server-started?*
       (open-client-window (~ win-controller'path) width height resizable?))
      (else
       (grv-start-server (not server-mode?)
                         (lambda (url)
                           (cond
                             (server-mode?
                              (log-info "Graviton server is running at")
                              (log-info "~a" url))
                             (use-player?
                              (invoke-player url width height (~ win'show?) resizable?))
                             (else
                              (open-browser url)))))))))

(define-window-context-slot window-element-alist '())

(define-syntax with-window
  (syntax-rules ()
    ((_ win (elements ...) body ...)
     (%with-window win
       (lambda ()
         (let-elements (elements ...)
           body ...))))))

(define (open-window win)
  (unless (window-context)
    (error "window-context not found"))
  (let* ((queue (make-mtqueue))
         (win-controller (make-window-controller #f (~ win'page) (lambda ()
                                                                   (let1 proc (dequeue/wait! queue)
                                                                     (proc (current-worker)))))))
    (open-client-window (~ win-controller'path) (~ win'width) (~ win'height) (~ win'resizable?))
    (shift-callback callback
      (enqueue! queue callback))))

(define (close-window :optional (ctx #f))
  (let1 ctx (or ctx (window-context))
    (unless (window-context)
      (error "window-context not found"))
    ((window-context-slot-ref ctx 'main-worker) %close-window-message)
    (undefined)))
