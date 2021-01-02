;;;
;;; grut.scm - Graviton Utility Toolkit
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

(define-module graviton.grut
  (use graviton)
  (use graviton.app)
  (use graviton.jsffi)
  (use graviton.misc)
  (use text.html-lite)
  (use util.list)
  (use util.match)

  (extend graviton.grut.audio graviton.grut.text)

  (export define-grut-window

          load-image
          load-audio))

(select-module graviton.grut)

(import-js ("/_g/grut/grut.mjs" :as Grut))
(load-css "/_g/grut/grut.css")

(define-application-context-slot content-element-table (make-hash-table 'eq?))

(define next-gen-id (make-id-generator))

(define (keywords->style keywords)
  (let loop ((rest keywords)
             (styles '()))
    (match rest
      (()
       (string-join styles ";"))
      (((? keyword? kw) v rest ...)
       (loop rest (cons (format "~a:~a" (keyword->string kw) v) styles)))
      (_
       (errorf "malformed keywords: ~s" rest)))))

(define (transform-element-spec rename spec)
  (define (id-keyword id)
    (if id
      (quasirename rename `(:id ,(symbol->string id)))
      '()))

  (define (class-keyword fit classes)
    (quasirename rename `(:class (string-join (cons (case ,fit
                                                      ((,'contain #f)
                                                       "grut-object-fit-contain")
                                                      ((,'cover)
                                                       "grut-object-fit-cover")
                                                      ((,'fill)
                                                       "grut-object-fit-fill")
                                                      ((,'none)
                                                       "grut-object-fit-none")
                                                      (else
                                                       (errorf "Invalid :fit option: ~s" ,fit)))
                                                    ',classes)
                                              " "))))
  (match spec
    (('text opts ...)
     (let-keywords opts ((id #f)
                         (fit #f)
                         (width #f)
                         (height #f)
                         . rest)
       (values
         (quasirename rename
           `(html:grv-text ,@(id-keyword id)
                           ,@(class-keyword fit '("grut-monospace-font" "grut-text"))
                           :data-width ,width
                           :data-height ,height
                           :style (keywords->style (list ,@rest))))
         (if id
           (quasirename rename
             `(define ,id (make-jsobject-singleton-provider (lambda ()
                                                              (document'get-element-by-id (symbol->string ',id))))))
           #f))))
    (('text-edit opts ...)
     (let-keywords opts ((id #f)
                         (fit #f)
                         (width #f)
                         (height #f)
                         . rest)
       (values
         (quasirename rename
           `(html:grv-text-edit ,@(id-keyword id)
                                ,@(class-keyword fit '("grut-monospace-font" "grut-text"))
                                :data-width ,width
                                :data-height ,height
                                :style (keywords->style (list ,@rest))))
         (if id
           (quasirename rename
             `(define ,id (make-jsobject-singleton-provider (lambda ()
                                                              (document'get-element-by-id (symbol->string ',id))))))
           #f))))
    (('canvas opts ...)
     (let-keywords opts ((id #f)
                         (fit #f)
                         (context-2d #f)
                         (width #f)
                         (height #f)
                         . rest)
       (let1 gen-canvas-id (string->symbol (format "_canvas:~a" (next-gen-id)))
         (values
           (quasirename rename
             `(html:canvas ,@(id-keyword (or id gen-canvas-id))
                           ,@(class-keyword fit '("grut-canvas"))
                           :width ,width
                           :height ,height
                           :style (keywords->style (list ,@rest))))
           (let* ((canvas (make-jsobject-singleton-provider
                            (lambda ()
                              (document'get-element-by-id (symbol->string (or id gen-canvas-id))))))
                  (ctx2d (make-jsobject-singleton-provider
                           (lambda ()
                             (canvas'get-context "2d")))))
             (quasirename rename
               `(begin
                  ,(if id
                     (quasirename rename `(define ,id ,canvas))
                     #f)
                  ,(if context-2d
                     (quasirename rename `(define ,context-2d ,ctx2d))
                     #f))))))))
    (_
     (errorf "malformed element spec: ~s" spec))))

(define-syntax define-grut-window
  (er-macro-transformer
    (lambda (form rename id=?)
      (let loop ((form (cdr form))
                 (elements '())
                 (top-levels '()))
        (match form
          (((? list? element-spec) rest ...)
           (receive (element top-level) (transform-element-spec rename element-spec)
             (loop rest (cons element elements) (cons top-level top-levels))))
          (opts
           (let-keywords opts ((theme #f)
                               (title #f)
                               . rest)
             (quasirename rename
               `(begin
                  (document-body-provider-set!
                    (lambda ()
                      (html:body :class (string-join
                                          (append
                                            (list (case ,theme
                                                    ((,'light)
                                                     "grut-light")
                                                    ((,'dark)
                                                     "grut-dark")
                                                    ((#f)
                                                     "")
                                                    (else
                                                     (errorf "Invalid theme: ~s" ,theme))))
                                            '("grut-body"))
                                          " ")
                                 :style (keywords->style (list ,@rest))
                                 ,@elements)))
                  (when ,title
                    (grv-title-set! ,title))
                  ,@top-levels)))))))))

;;;

(define (load-image url :key (content-type #f) (on-error :error))
  (or (jslet/result ((url::string))
        (let ((img (make Image)))
          (set! img.src url)
          (set! img.onload (lambda ()
                             (set! img.onload undefined)
                             (set! img.onerror undefined)
                             (result img)))
          (set! img.onerror (lambda ()
                              (set! img.onload undefined)
                              (set! img.onerror undefined)
                              (result #f)))))
      (case on-error
        ((#f) #f)
        ((:error) (errorf "Failed to load image: ~a" url))
        (else
         (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error)))))

(define (load-audio url :key (content-type #f) (on-error :error))
  (receive (audio err) (jslet/result ((url::string))
                         (let1 audio (make Audio url)
                           (set! audio.onloadeddata (lambda ()
                                                      (set! audio.onloadeddata undefined)
                                                      (set! audio.onerror undefined)
                                                      (result audio #f)))
                           (set! audio.onerror (lambda ()
                                                 (set! audio.onloadeddata undefined)
                                                 (set! audio.onerror undefined)
                                                 (result #f audio.error.message)))))
    (or audio
        (case on-error
          ((#f) #f)
          ((:error) (errorf "Failed to load audio: ~a (~a)" url err))
          (else
           (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error))))))

