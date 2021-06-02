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

  (extend graviton.async graviton.browser-objects graviton.event graviton.canvas graviton.audio)

  (export grv-exit

          grv-start-player
          grv-start-server

          grv-title
          grv-title-set!

          grv-log-config

          autoload-css
          bind-url-path

          flush-client-request

          make-grv-window
          grv-window
          let-elements

          <window-parameter>
          make-window-parameter
          make-window-parameter*
          window-parameter-atomic-ref
          window-parameter-atomic-update!

          query-parameters

          define-global-jsobject

          make-jsobject

          user-agent
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

(define (autoload-css-list module)
  (let1 css-list (append (apply append (map autoload-css-list (module-imports module)))
                         (apply append (map autoload-css-list (module-parents module)))
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
                     (reverse css-list))))))

;;;

(define *grv-default-title*
  (if (and (list? (command-line))
           (<= (length (command-line)) 1))
    (path-sans-extension (sys-basename (list-ref (command-line) 0)))
    "Graviton"))

(define (grv-title)
  (cond
    ((application-context)
     (~ document'title))
    (else
     *grv-default-title*)))

(define (grv-title-set! title)
  (cond
    ((application-context)
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
            (respond/ok req js-code :content-type "text/javascript"))
          (respond/ng req 404)))))

;;;

(define *path->window-table* (make-hash-table 'equal?))
(define window-id-generator (make-id-generator))

(define-class <grv-window> ()
  ((id :init-form (window-id-generator))
   (%path :init-value #f)
   (path :allocation :virtual
         :slot-ref (lambda (win)
                     (or (slot-ref win '%path)
                         (format "/_w/~a" (~ win'id))))
         :slot-set! (lambda (win path)
                      (cond
                        (path
                         (and-let1 w (lookup-grv-window path)
                           (slot-set! w 'path #f))
                         (slot-set! win '%path path))
                        (else
                         (and-let1 cpath (slot-ref win '%path)
                           (hash-table-delete! *path->window-table* cpath))
                         (slot-set! win '%path #f)))
                      (hash-table-put! *path->window-table* (slot-ref win 'path) win))
         :init-keyword :path)
   (page :init-keyword :page)
   (thunk :init-keyword :thunk)))

(define-method initialize ((win <grv-window>) initargs)
  (next-method))

(define (make-grv-window path page thunk)
  (make <grv-window> :path path :page page :thunk thunk))

(define (make-window-page module page-form)
  (let loop ((page-form page-form)
             (title *grv-default-title*)
             (css-list (autoload-css-list module))
             (js-list '())
             (head '())
             (body (html:body)))
    (match page-form
      (()
       (tree->string
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
                  (html:script :type "module" :src js-mod))
                (js-main-module-absolute-paths))
           (map (lambda (js)
                  (html:script :src js))
                js-list)
           (html:title title)
           head)
          body)))
      ((':title title rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body))
      ((':css (? list? css) rest ...)
       (for-each (^x (unless (string? x) (errorf "a list of <string> required, but got ~s" x))) css)
       (loop rest
             title
             (append css-list css)
             js-list
             head
             body))
      ((':css (? string? css) rest ...)
       (loop rest
             title
             (append css-list (list css))
             js-list
             head
             body))
      ((':js (? list js) rest ...)
       (for-each (^x (unless (string? x) (errorf "a list of <string> required, but got ~s" x))) js)
       (loop rest
             title
             css-list
             (append js-list js)
             head
             body))
      ((':js (? string? js) rest ...)
       (loop rest
             title
             css-list
             (append js-list (list js))
             head
             body))
      ((':head additional-head rest ...)
       (loop rest
             title
             css-list
             js-list
             (append head (list additional-head))
             body))
      ((':body body rest ...)
       (loop rest
             title
             css-list
             js-list
             head
             body))
      (_
       (errorf "Invalid page form: ~s" page-form)))))

(define-syntax grv-window
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ win-spec ...)
         (let loop ((win-spec win-spec)
                    (path #f)
                    (page-form '()))
           (match win-spec
             ((':path path rest ...)
              (loop rest path page-form))
             (((? keyword? kw) arg rest ...)
              (loop rest path (append page-form (list kw arg))))
             (body
              (quasirename rename
                `(make-grv-window ,path (make-window-page (current-module) (list ,@page-form)) (lambda () ,@body)))))))
        (_
         (errorf "malformed grv-window: ~s" form))))))

(define (lookup-grv-window path)
  (hash-table-get *path->window-table* (simplify-path (string-append "/" path)) #f))

;;;

(define-syntax let-elements
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ () body ...)
         (quasirename rename
           `(let () ,@body)))
        ((_ ((? symbol? id) rest ...) body ...)
         (quasirename rename
           `(let1 ,id (document'get-element-by-id ,(symbol->string id))
              (let-elements ,rest ,@body))))
        ((_ (((? symbol? var) name) rest ...) body ...)
         (quasirename rename
           `(let1 ,var (document'get-element-by-id ,name)
              (let-elements ,rest ,@body))))
        (_
         (errorf "malformed let-elements: ~s" form))))))

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

(define (start-websocket-dispatcher! ctx sock in out req-user-agent thunk)
  (thread-start!
    (make-thread
      (lambda ()
        (guard (e (else (report-error e)
                        (exit 70)))
          (atomic-update! *num-connection-atom* (^x (+ x 1)))
          (let1 exit-code (websocket-main-loop ctx in out req-user-agent thunk)
            (log-framework-debug "WebSocket dispatcher finished: ~a" exit-code)
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
    (let* ((in (request-iport req))
           (out (request-oport req))
           (win (lookup-grv-window (or ((~ req'path-rxmatch) 1) "/")))
           (thunk (and win (~ win'thunk))))
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
           (let1 ctx (make-application-context)
             (parameterize ((application-context ctx))
               (query-parameters (~ req'params)))
             (start-websocket-dispatcher! ctx (request-socket req) in out req-user-agent thunk))
           req)
          (else
           (respond/ng req 400)))))))

(define-http-handler #/.+/
  (lambda (req app)
    (let1 url-path (slot-ref req 'path)
      (cond
        ((lookup-grv-window url-path)
         => (lambda (win)
              (respond/ok req (~ win'page))))
        (else
         (receive (body content-type) (url-path->body&content-type url-path)
           (if body
             (respond/ok req body :content-type content-type)
             (respond/ng req 404))))))))

;;;


(define-method make-jsobject ((jsobject-class <jsobject-meta>) :rest args)
  (apply make-jsobject (~ jsobject-class'jsclass) args))

(define-method make-jsobject ((jsclass <string>) :rest args)
  (jslet/result ((jsclass::string)
                 (args (list->vector args)))
    (result (Graviton.makeJSObject jsclass args))))

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

(define *graviton-access-log-drain* #f)
(define *graviton-error-log-drain* #t)

(define *control-channel* (make-server-control-channel))

(define (server-url sock)
  (format "http://localhost:~a/" (sockaddr-port (socket-address sock))))

(define (invoke-player config url)
  (let* ((config-file (generate-player-config-file config url))
         (player-path (graviton-config 'graviton-player-path))
         (stdout-filename (~ config'stdout-filename))
         (stderr-filename (~ config'stderr-filename)))
    (cond
      ((file-exists? player-path)
       (run-process `(,player-path "--config" ,config-file)
                    :output (~ config'stdout-filename)
                    :error (~ config'stderr-filename)))
      (else
       (run-process `("npx" "electron" "." "--config" ,config-file)
                    :directory "./player/src"
                    :output (~ config'stdout-filename)
                    :error (~ config'stderr-filename))))))

(define (generate-player-config-file config url)
  (let* ((win-size (~ config'window-size))
         (config `((width . ,(list-ref win-size 0))
                   (height . ,(list-ref win-size 1))
                   (url . ,url)
                   (open-dev-tools . ,(~ config'open-dev-tools?))
                   (show . ,(~ config'show?))
                   (resizable . ,(~ config'resizable?))))
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

(define (%grv-start-server root-win
                           shutdown-if-all-connection-closed?
                           startup-callback
                           :key
                           (port #f)
                           (access-log #f)
                           (error-log #f))
  (set! *shutdown-if-all-connection-closed?* shutdown-if-all-connection-closed?)
  (when root-win
    (slot-set! root-win 'path "/"))
  (start-http-server :port port
                     :access-log access-log
                     :error-log error-log
                     :control-channel *control-channel*
                     :startup-callback (lambda (socks)
                                         (startup-callback (server-url (first socks))))))

(define (grv-start-player :key
                          (root #f)
                          (port 0)
                          (access-log #f)
                          (error-log #f)
                          window-size
                          open-dev-tools?
                          show?
                          resizable?
                          stdout-filename
                          stderr-filename)
  (let1 config (config-with-params <player-config>
                 window-size
                 open-dev-tools?
                 show?
                 resizable?
                 stdout-filename
                 stderr-filename)
    (%grv-start-server root
                       #t
                       (lambda (url)
                         (invoke-player config url))
                       :port port
                       :access-log access-log
                       :error-log error-log)))

(define (grv-start-server :key
                          (root #f)
                          (port 8080)
                          (access-log #f)
                          (error-log #f)
                          (open? #f))
  (%grv-start-server root
                     #f
                     (lambda (url)
                       (log-info "Graviton server is running at")
                       (log-info "~a" url)
                       (when open?
                         (open-browser url)))
                     :port port
                     :access-log access-log
                     :error-log error-log))

(define (grv-exit :optional (code 0))
  (flush-client-request)
  (app-exit code))
