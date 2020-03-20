;;;
;;; config.scm - Graviton config parameter
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

(define-module graviton.config
  (use file.util)
  (use gauche.regexp)
  (use util.list)

  (export graviton-config))

(select-module graviton.config)

(load "graviton/config-alist.scm")

(define *graviton-installed?*
  (and (current-load-path)
       (equal? (gauche-site-library-directory) (sys-dirname (current-load-path)))))

(define *graviton-top-dir* (simplify-path (build-path (sys-dirname (current-load-path)) ".." "..")))

(define (replace-param val)
  (cond
    ((string? val)
     (rxmatch-if (#/\${([^\}]+)}/ val)
         (expand-place param)
       (replace-param (regexp-replace-all (string->regexp (regexp-quote expand-place))
                                          val
                                          (graviton-config (string->symbol param))))
       val))
    (else
     val)))

(define (graviton-config param)
  (cond
    ((and (equal? param 'graviton-data-dir)
          (not *graviton-installed?*))
     *graviton-top-dir*)
    (else
     (replace-param (assoc-ref *config-alist* param)))))

