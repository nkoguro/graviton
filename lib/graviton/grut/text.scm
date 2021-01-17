;;;
;;; text.scm - Graviton Text element
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

(define-module graviton.grut.text
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.app)
  (use graviton.browser-objects)
  (use graviton.jsffi)
  (use text.html-lite)
  (use util.match)

  (export html:grv-text
          html:grv-text-edit
          <event-map>
          make-key-map
          grv-text-edit-key-map
          grv-text-key-map
          <grv-text>
          <grv-text-edit>
          call-with-output-grv-text
          with-output-to-grv-text))

(select-module graviton.grut.text)

(import-js ("/_g/grut/text.mjs" :as Text))

;;;

;; Copied from text.html-lite.
(define (make-html-element name . args)
  (let ((empty? (get-keyword :empty? args #f)))
    (define (K k) (keyword->string k)) ;; we don't need leading colon
    (define (get-attr args attrs)
      (cond ((null? args) (values (reverse attrs) args))
            ((keyword? (car args))
             (cond ((null? (cdr args))
                    (values (reverse (list* (K (car args)) " " attrs)) args))
                   ((eq? (cadr args) #f)
                    (get-attr (cddr args) attrs))
                   ((eq? (cadr args) #t)
                    (get-attr (cddr args) (list* (K (car args)) " " attrs)))
                   (else
                    (get-attr (cddr args)
                              (list* (format #f "=\"~a\""
                                             (html-escape-string (x->string (cadr args))))
                                     (K (car args))
                                     " "
                                     attrs)))))
            (else (values (reverse attrs) args))))

    (if empty?
      (lambda args
        (receive (attr args) (get-attr args '())
          (unless (null? args)
            (errorf "element ~s can't have content: ~s" name args))
          (list "<" name attr " />")))
      (lambda args
        (receive (attr args) (get-attr args '())
          (list "<" name attr ">" args "</" name "\n>"))))))

(define-macro (define-html-elements . elements)
  (define (make-scheme-name name)
    (string->symbol (format #f "html:~a" name)))
  (let loop ((elements elements)
             (r '()))
    (cond ((null? elements) `(begin ,@(reverse r)))
          ((and (pair? (cdr elements)) (eqv? (cadr elements) :empty))
           (loop (cddr elements)
                 (list* `(define ,(make-scheme-name (car elements))
                           (make-html-element ',(car elements) :empty? #t))
                        `(export ,(make-scheme-name (car elements)))
                        r)))
          (else
           (loop (cdr elements)
                 (list* `(define ,(make-scheme-name (car elements))
                           (make-html-element ',(car elements)))
                        `(export ,(make-scheme-name (car elements)))
                        r))))
    ))

(define-html-elements grv-text grv-text-edit)

;;;

(define-class <event-map> (<jsobject>)
  ()
  :jsclass "EventMap")

(define (make-key-map :optional (parent #f))
  (jslet/result ((parent))
    (if parent
      (result (make Text.EventMap parent))
      (result (make Text.EventMap null)))))

(define-automatic-jsobject-methods <event-map>
  "bind")

(define-class <grv-abstract-text> (<html-element>)
  ((last-update-timestamp :jsproperty "lastUpdateTimestamp"
                          :read-only? #t))
  :jsclass "GrvAbstractText")

(define-global-jsobject grv-text-edit-key-map (jslet/result ()
                                                (result Text.SCREEN_KEYMAP)))
(define-global-jsobject grv-text-key-map (jslet/result ()
                                           (result Text.TERMINAL_KEYMAP)))

(define-class <grv-text> (<grv-abstract-text>)
  ((ediable :jsproperty "editable"
            :read-only? #t))
  :jsclass "GrvText")

(define-class <grv-text-edit> (<grv-abstract-text>)
  ((text-content :jsproperty "textContent"))
  :jsclass "GrvTextEdit")

(define-automatic-jsobject-methods <grv-abstract-text>
  "bindKey"
  "callCommand"
  "insertText"
  "printText")

(define-jsobject-method <grv-abstract-text> cursor-column ()
  (jslet/result ((self::object))
    (result self.cursor.column)))

(define-jsobject-method <grv-abstract-text> cursor-row ()
  (jslet/result ((self::object))
    (result self.cursor.row)))

(define-jsobject-method <grv-abstract-text> show-cursor ()
  (jslet ((self::object))
    (self.cursor.show)))

(define-jsobject-method <grv-abstract-text> hide-cursor ()
  (jslet ((self::object))
    (self.cursor.hide)))

(define-automatic-jsobject-methods <grv-text>
  "completeLineEdit")

(define-jsobject-method <grv-text> read-line (:key (prompt "") (focus? #t) (content "") (position 0))
  (jslet/result ((self::object)
                 (prompt::string)
                 (focus?)
                 (content::string)
                 (position))
    (self.readLine (lambda (content key)
                     (result content key))
                   prompt
                   focus?
                   content
                   position)))

(define-automatic-jsobject-methods <grv-text-edit>
  "clearTextContent")

(define (compute-output-length buf)
  (let1 len (u8vector-length buf)
    (let loop ((i 0))
      (let* ((c (u8vector-ref buf i))
             (char-len (cond
                         ((= (ash c -5) #b110)
                          2)
                         ((= (ash c -4) #b1110)
                          3)
                         ((= (ash c -3) #b11110)
                          4)
                         (else
                          1))))
      (cond
        ((< len (+ i char-len))
         i)
        ((= len (+ i char-len))
         (+ i char-len))
        (else
         (loop (+ i char-len))))))))


(define (get-output-grv-text text-element :key (buffer-size 0))
  (rlet1 out (make <buffered-output-port>
               :flush (let1 pending-buf #u8()
                            (lambda (buf flag)
                              ;; If flush is called outside worker-thread (e.g. it can be called by GC), discard the buffer content.
                              (when (and (application-context) (< 0 (u8vector-length buf)))
                                (let* ((data (u8vector-append pending-buf buf))
                                       (out-len (compute-output-length data)))
                                  (when (< 0 out-len)
                                    (text-element'print-text (u8vector->string (u8vector-copy data 0 out-len))))
                                  (set! pending-buf (u8vector-copy data out-len))))
                              (u8vector-length buf)))
               :buffer-size buffer-size)
    (set! (port-buffering out) :line)))

(define (call-with-output-grv-text text-element proc :key (buffer-size 0))
  (let1 out (get-output-grv-text text-element :buffer-size buffer-size)
    (unwind-protect
        (proc out)
      (close-output-port out))))
