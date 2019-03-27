;;;
;;; repl.scm - REPL
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

(define %evaluator
  (let ((%set-history-exception! (with-module gauche.interactive %set-history-exception!))
        (%set-history-expr! (with-module gauche.interactive %set-history-expr!)))
    (lambda (sexpr mod)
      (guard (e (else (%set-history-exception! e)
                      (raise e)))
        (receive r (eval sexpr mod)
          (%set-history-expr! r)
          (apply values r))))))

(define %prompter
  (let1 user-module (find-module 'user)
    (lambda ()
      (let1 m ((with-module gauche.internal vm-current-module))
        (if (eq? m user-module)
            (display "graviton> ")
            (format #t "graviton[~a]> " (module-name m)))
        (flush)))))

(define %printer
  (with-module gauche.interactive
    %printer))

(define (%reader)
  (cond
    ((event-loop-running?)
     (await (async/thread
              (match (read)
                (('unquote command)
                 (with-module gauche.interactive
                   (handle-toplevel-command command (read-line))))
                (expr
                 (unless (eof-object? expr)
                   (consume-trailing-whitespaces))
                 expr)))))
    (else
     (eof-object))))

(define (grv-repl)
  (grv-main
    (lambda ()
      (read-eval-print-loop %reader %evaluator %printer %prompter))))


