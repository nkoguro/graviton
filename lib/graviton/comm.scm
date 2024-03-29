;;;
;;; comm.scm - Communication
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

(define-module graviton.comm
  (use binary.io)
  (use data.queue)
  (use gauche.hook)
  (use gauche.parameter)
  (use gauche.selector)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.app)
  (use graviton.async)
  (use graviton.misc)
  (use makiki)
  (use rfc.json)
  (use srfi-1)
  (use srfi-27)
  (use util.match)

  (export client-request-output
          flush-client-request
          with-client-request

          websocket-main-loop

          define-action
          register-binary-handler!))

(select-module graviton.comm)

;;;

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

(define (send-pong-frame out data)
  (send-frame out #xa data))

;;;

(define %client-request-output (make-thread-local #f))

(define (client-request-output)
  (or (tlref %client-request-output)
      (begin
        (tlset! %client-request-output (open-output-uvector))
        (tlref %client-request-output))))

(define (flush-client-request)
  (window-context-slot-atomic-ref (window-context) 'websocket-output-port
    (lambda (wout)
      (let1 data (get-output-uvector (client-request-output) :shared #t)
        (when (< 0 (u8vector-length data))
          (tlset! %client-request-output (open-output-uvector))
          (send-binary-frame wout data))))))

(define (discard-client-request)
  (tlset! %client-request-output (open-output-uvector)))

(define (with-client-request thunk)
  (let1 out (tlref %client-request-output)
    (dynamic-wind
        (lambda ()
          (tlset! %client-request-output (open-output-uvector)))
        (lambda ()
          (guard (e (else (discard-client-request)
                          (raise e)))
            (thunk)))
        (lambda ()
          (flush-client-request)
          (tlset! %client-request-output out)))))

(add-hook! worker-process-event-start-hook flush-client-request)

;;;

(define-class <websocket-server-context> ()
  ((continuation-opcode :init-value 0)
   (continuation-frames :init-value '())))

(define (reset-context! ctx)
  (slot-set! ctx 'continuation-opcode 0)
  (slot-set! ctx 'continuation-frames '()))

(define-condition-type <websocket-end-of-data> <condition> #f)

(define (read-websocket-header in)
  (let1 header (read-u16 in 'big-endian)
    (cond
      ((eof-object? header)
       (raise (condition (<websocket-end-of-data>))))
      (else
       (let* ((fin? (logbit? 15 header))
              (opcode (logand (ash header -8) #b1111))
              (mask? (logbit? 7 header))
              (payload-length
               (let1 v (logand header #b1111111)
                 (cond
                   ((< v 126)
                    v)
                   ((= v 126)
                    (read-u16 in 'big-endian))
                   ((= v 127)
                    (read-u64 in 'big-endian))))))
         (cond
           ((eof-object? payload-length)
            (raise (condition (<websocket-end-of-data>))))
           (else
            (values fin? opcode mask? payload-length))))))))

(define (read-websocket-masking-key mask? in)
  (let1 masking-key (if mask?
                        (read-uvector <u8vector> 4 in)
                        #u8(0 0 0 0))
    (cond
      ((eof-object? masking-key)
       (raise (condition (<websocket-end-of-data>))))
      (else
       masking-key))))

(define (read-websocket-payload-data payload-length masking-key in)
  (let1 payload-data (make-u8vector payload-length)
    (let1 i 0
      (while (and (< i payload-length)
                  (read-uvector! payload-data in i -1 'little-endian))
        => count
        (cond
          ((eof-object? count)
           (raise (condition (<websocket-end-of-data>))))
          (else
           (inc! i count))))

      (dotimes (i payload-length)
        (u8vector-set! payload-data
                       i
                       (logxor (u8vector-ref payload-data i)
                               (u8vector-ref masking-key (modulo i 4)))))
      payload-data)))

(define (handle-payload ctx
                        in
                        out
                        close-handler)
  (guard (e ((<websocket-end-of-data> e) (close-handler)))
    (receive (fin? opcode mask? payload-length) (read-websocket-header in)
      (let* ((masking-key (read-websocket-masking-key mask? in))
             (payload-data (read-websocket-payload-data payload-length masking-key in)))
        (log-framework-debug "receive frame: fin?=~a, opcode=~a, payload-length=~a" fin? opcode payload-length)
        (case opcode
          ((0)
           (cond
             (fin?
              (let ((cont-opcode (slot-ref ctx 'continuation-opcode))
                    (cont-frames (cons payload-data (slot-ref ctx 'continuation-frames))))
                (reset-context! ctx)
                (case cont-opcode
                  ((1)
                   (receive-json ctx (u8vector->string (apply u8vector-append (reverse cont-frames)))))
                  ((2)
                   (receive-binary ctx (apply u8vector-append (reverse cont-frames)))))))
             (else
              (push! (slot-ref ctx 'continuation-frames) payload-data))))
          ((1)
           (cond
             (fin?
              (receive-json ctx (u8vector->string payload-data)))
             (else
              (slot-set! ctx 'continuation-opcode opcode)
              (slot-set! ctx 'continuation-frames (list payload-data)))))
          ((2)
           (cond
             (fin?
              (receive-binary ctx payload-data))
             (else
              (slot-set! ctx 'continuation-opcode opcode)
              (slot-set! ctx 'continuation-frames (list payload-data)))))
          ((8)
           (let ((status (get-u16 payload-data 0 'big-endian))
                 (rest (uvector-alias <u8vector> payload-data 2 (u8vector-length payload-data))))
             (log-framework-debug "WebSocket closed: code=~a, data=(~a)" status rest)
             (close-handler)))
          ((9)
           (log-framework-debug "Received ping frame: ~s" payload-data)
           (send-pong-frame out payload-data))
          ((10)
           (log-framework-debug "Received pong frame: ~s" payload-data)))))))

;;;

(define json-command-table (make-hash-table 'equal?))

(define (handle-json-special sym)
  (case sym
    ((false) #f)
    ((true) #t)
    (else sym)))

(define (receive-json ctx json-str)
  (let* ((params (vector->list (parameterize ((json-special-handler handle-json-special))
                                 (parse-json-string json-str))))
         (cmd (car params))
         (args (cdr params))
         (proc (hash-table-get json-command-table cmd #f)))
    (cond
      (proc
       (worker-submit-task (main-worker ctx) (cut apply proc args)))
      (else
       (log-error "Invalid data received: ~s" params)))))

(define-syntax define-action
  (syntax-rules ()
    ((_ action-name args body ...)
     (hash-table-put! json-command-table action-name (lambda args body ...)))))


;;;

(define *binary-handler* #f)

(define (register-binary-handler! proc)
  (set! *binary-handler* proc))

(define (receive-binary ctx data)
  (and-let1 proc *binary-handler*
    (proc ctx data)))

;;;

(define-window-context-slot websocket-output-port #f)

(define (websocket-main-loop ctx in out req-user-agent thunk)
  (let1 exit-code 0

    (window-context-slot-set! ctx 'websocket-output-port out)
    (window-parameter-set! ctx user-agent req-user-agent)

    (receive (ctrl-in ctrl-out) (sys-pipe)
      (window-context-slot-set! ctx 'control-out ctrl-out)

      (let ((sel (make <selector>))
            (run-loop? #t))
        (define (exit-loop)
          (set! run-loop? #f))

        (selector-add! sel
                       ctrl-in
                       (lambda (in flag)
                         (match (read in)
                           (('shutdown code)
                            (set! exit-code code)
                            (exit-loop))
                           (cmd
                            (errorf "Invalid control command: ~s" cmd))))
                       '(r))
        (selector-add! sel
                       in
                       (lambda (in flag)
                         (handle-payload ctx in out exit-loop))
                       '(r))

        ;; Invoke main worker thread
        (let1 wt (make-worker ctx thunk :name "main")
          (worker-run wt)
          (window-context-slot-set! ctx 'main-worker wt))

        (while run-loop?
          (selector-select sel))

        (selector-delete! sel #f #f #f)))

    (let1 workers (all-workers ctx)
      (for-each (cut worker-shutdown ctx <>) workers))

    (window-context-invalidate! ctx)

    exit-code))
