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
  (use data.queue)
  (use gauche.generator)
  (use gauche.hook)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.app)
  (use graviton.async)
  (use graviton.browser-objects)
  (use graviton.jsffi)
  (use graviton.misc)
  (use srfi-1)
  (use text.html-lite)
  (use util.match)

  (export html:grv-text
          clipboard-text
          <grv-text>
          clear-input-buffer!
          call-with-output-grv-text
          with-output-to-grv-text
          enable-screen-edit
          read-string-from-grv-text))

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

(define-html-elements grv-text)

;;;

(define-class <grv-text> (<html-element>)
  ((text-content :jsproperty "textContent")
   (content-version :jsproperty "contentVersion")
   (%text-input-handler :init-value #f)
   (text-input-handler :allocation :virtual
                       :slot-ref (lambda (obj)
                                   (slot-ref obj '%text-input-handler))
                       :slot-set! (lambda (obj handler)
                                    (obj'enable-input-event (if handler #t #f))
                                    (slot-set! obj '%text-input-handler handler)))
   (keymap :init-form (make-hash-table 'equal?))
   (processor-state :init-value #f)
   (pre-key-handler-hook :init-form (make-hook 1))
   (post-key-handler-hook :init-form (make-hook 1))
   (pre-input-handler-hook :init-form (make-hook 1))
   (post-input-handler-hook :init-form (make-hook 1))
   (input-queue :init-form (make-mtqueue))

   ;; for line editing
   (line-edit-cont :init-value #f)
   (start-column-table :init-form (make-hash-table 'eqv?))
   (input-line-table :init-form (make-hash-table 'eqv?))
   (start-row :init-value #f)
   (end-row :init-value #f))
  :jsclass "GrvText")

(define clipboard-text (make-parameter #f))

(define-method initialize ((self <grv-text>) initargs)
  (next-method)
  (self'set-callbacks
   (lambda (str)
     (and-let1 proc (slot-ref self 'text-input-handler)
       (run-hook (~ self'pre-input-handler-hook) str)
       (proc str)
       (run-hook (~ self'post-input-handler-hook) str)))
   (lambda (key clipboard)
     (parameterize ((clipboard-text clipboard))
       (run-hook (~ self'pre-key-handler-hook) key)
       (and-let1 thunk (hash-table-get (~ self'keymap) key #f)
         (thunk))
       (run-hook (~ self'post-key-handler-hook) key))))
  (slot-set! self 'processor-state (make-text-processor self)))

(define-automatic-jsobject-methods <grv-text>
  "setCallbacks"
  "enableInputEvent"
  "setKeyEventAvailability"
  "clearKeyEventAvailability"
  "pushKeyEventAvailability"
  "popKeyEventAvailability"
  "setMoveCursorByMouseClick"

  "setTextAttribute"
  "setColor"
  "setForegroundColor"
  "setBackgroundColor"
  "setFontStyle"
  "setTextDecoration"
  "invertTextColor"
  "updateLineAttribute"

  ("getText" :result #t)
  ("getLine" :result #t)

  "writeLine"
  "eraseDisplay"
  "eraseLine"
  "splitLine"
  "concatLine"
  "removeAllLines"

  "moveCursor"
  "moveCursorUp"
  "moveCursorDown"
  "moveCursorForward"
  "moveCursorBack"
  "moveCursorNextLine"
  "moveCursorPreviousLine"
  "moveCursorHorizontalAbsolute"

  "forwardChar"
  ;; "previousChar"
  "nextLine"
  "previousLine"
  ;; "beginningOfLine"
  "endOfLine"
  "beginningOfBuffer"
  "endOfBuffer"
  "pageUp"
  "pageDown"

  "scrollUp"
  "scrollDown"

  ;; "backwardDeleteChar"
  "deleteChar"

  ("extractMarkRegionText" :result #t)
  "deleteMarkRegion"
  "copyMarkRegion"
  "cutMarkRegion"

  "setMark"
  "clearMark"
  "toggleMark"
  )

(define-jsobject-method <grv-text> cursor-column ()
  (jslet/result ((self::object))
    (result self.cursor.column)))

(define-jsobject-method <grv-text> cursor-row ()
  (jslet/result ((self::object))
    (result self.cursor.row)))

(define-jsobject-method <grv-text> show-cursor ()
  (jslet ((self::object))
    (self.cursor.show)))

(define-jsobject-method <grv-text> hide-cursor ()
  (jslet ((self::object))
    (self.cursor.hide)))

(define-jsobject-method <grv-text> cursor-visible? ()
  (jslet/result ((self::object))
    (result self.cursor.visible)))

(define (get-start-column grv-text :optional (row #f))
  (let1 tbl (~ grv-text'start-column-table)
    (hash-table-get tbl (or row (grv-text'cursor-row)) #f)))

(define (set-start-column! grv-text row v)
  (let1 tbl (~ grv-text'start-column-table)
    (if v
      (hash-table-put! tbl row v)
      (hash-table-delete! tbl row))))

(define (at-start-column? grv-text)
  (let1 start-col (get-start-column grv-text)
    (and start-col
         (<= (grv-text'cursor-column) start-col))))

(define-jsobject-method <grv-text> previous-char (n :optional (shift-mark #f))
  (unless (at-start-column? self)
    (jslet ((self::object)
            (n)
            (shift-mark))
      (self.previousChar n shift-mark))))

(define-jsobject-method <grv-text> beginning-of-line (:optional (shift-mark #f))
  (jslet ((self::object)
          (shift-mark)
          (start-column (or (get-start-column self) 0)))
    (self.beginningOfLine shift-mark start-column)))

(define-jsobject-method <grv-text> backward-delete-char (:optional (n 1))
  (unless (at-start-column? self)
    (jslet ((self::object)
            (n))
      (self.backwardDeleteChar n))))

(define (enqueue-input grv-text str)
  (enqueue! (~ grv-text'input-queue) str))

(define (dequeue-input grv-text :optional (wait? #f))
  (if wait?
    (dequeue/wait! (~ grv-text'input-queue))
    (dequeue! (~ grv-text'input-queue) #f)))

(define (clear-input-buffer! grv-text)
  (dequeue-all! (~ grv-text'input-queue))
  (undefined))

;;;

(define (parse-basic-color n)
  (vector-ref #("black"
                "maroon"
                "green"
                "olive"
                "navy"
                "purple"
                "teal"
                "silver"
                "gray"
                "red"
                "lime"
                "yellow"
                "blue"
                "fuchsia"
                "aqua"
                "white")
              n
              #f))

(define (parse-color params)
  (match params
    ((5 n rest ...)
     (cond
       ((<= 0 n 15)
        (values (parse-basic-color n) rest))
       ((<= 16 n 231)
        (let* ((v (- n 16))
               (b (modulo v 6))
               (g (modulo (/ (- v b) 6) 6))
               (r (/ (- v b (* 6 g)) 36)))
          (values (format "#~2,'0x~2,'0x~2,'0x"
                          (ceiling->exact (* (/ 255 6) (+ r 1)))
                          (ceiling->exact (* (/ 255 6) (+ g 1)))
                          (ceiling->exact (* (/ 255 6) (+ b 1))))
                  rest)))
       ((<= 232 n 255)
        (let1 v (ceiling->exact (* (/ 255 24) (- n 231)))
          (values (format "#~2,'0x~2,'0x~2,'0x" v v v) rest)))
       (else
        (values #f rest))))
    ((2 r g b rest ...)
     (values (format "#~2,'0x~2,'0x~2,'0x" r g b) rest))
    (_
     (values #f params))))

(define (make-text-processor grv-text)
  (let ((overwrite? #f)
        (string-port #f)
        (data '())
        (params '())
        (intensity? #f)
        (reversed? #f))

    (define (output-byte b)
      (unless string-port
        (set! string-port (open-output-string :name (format "(~s text-processor)" grv-text))))
      (write-byte b string-port))

    (define (flush-string)
      (when string-port
        (grv-text'write-line (get-output-string string-port) overwrite?))
      (set! string-port #f))

    (define (hold-param-data b)
      (push! data b))

    (define (push-param)
      (cond
        ((null? data)
         (push! params #f))
        (else
         (push! params (string->number (apply string (map ucs->char (reverse data)))))))
      (set! data '()))

    (define (get-param n fallback)
      (or (list-ref params n #f) fallback))

    (define (clear-params)
      (set! data '())
      (set! params '()))

    (define (update-style)
      (let loop ((params (reverse params)))
        (cond
          ((null? params)
           #t)
          (else
           (case (car params)
             ((0 #f)                    ; Reset/Normal
              (grv-text'set-foreground-color #f)
              (grv-text'set-background-color #f)
              (grv-text'set-font-style #f)
              (grv-text'set-text-decoration #f)
              (set! intensity? #f)
              (set! reversed? #f)
              (loop (cdr params)))
             ((1)                       ; Increased intensity
              (set! intensity? #t)
              (loop (cdr params)))
             ((2)                       ; Decreased intensity
              (set! intensity? #f)
              (loop (cdr params)))
             ((3)                       ; Italic
              (grv-text'set-font-style "italic")
              (loop (cdr params)))
             ((4)                       ; Underline
              (grv-text'set-text-decoration "underline")
              (loop (cdr params)))
             ((7)                       ; Reverse video
              (unless reversed?
                (grv-text'invert-text-color)
                (set! reversed? #t))
              (loop (cdr params)))
             ((22)                      ; Normal color or intensity
              (grv-text'set-foreground-color #f)
              (grv-text'set-background-color #f)
              (set! reversed? #f)
              (set! intensity? #f)
              (loop (cdr params)))
             ((23)                      ; Not italic
              (grv-text'set-font-style #f)
              (loop (cdr params)))
             ((24)                      ; Underline off
              (grv-text'set-text-decoration #f)
              (loop (cdr params)))
             ((27)                      ; Reverse off
              (when reversed?
                (grv-text'invert-text-color)
                (set! reversed? #f))
              (loop (cdr params)))
             ((30 31 32 33 34 35 36 37) ; Set foreground color
              (grv-text'set-foreground-color (parse-basic-color (+ (- (car params) 30) (if intensity? 8 0))))
              (loop (cdr params)))
             ((38)                ; Set foreground color (8 or 24 bit)
              (receive (color rest) (parse-color (cdr params))
                (grv-text'set-foreground-color color)
                (loop rest)))
             ((39)                      ; Default foreground color
              (grv-text'set-foreground-color #f))
             ((40 41 42 43 44 45 46 47) ; Set background color
              (grv-text'set-background-color (parse-basic-color (+ (- (car params) 40) (if intensity? 8 0))))
              (loop (cdr params)))
             ((48)                ; Set background color (8 or 24 bit)
              (receive (color rest) (parse-color (cdr params))
                (grv-text'set-background-color color)
                (loop rest)))
             ((49)                      ; Default background color
              (grv-text'set-background-color #f))
             (else
              (loop (cdr params))))))))

    (define (make-state proc)
      (lambda (b flag)
        (set! overwrite? flag)
        (slot-set! grv-text 'processor-state (cond
                                               ((eof-object? b)
                                                (flush-string)
                                                char-state)
                                               (else
                                                (proc b))))))

    (define char-state
      (make-state (lambda (b)
                    (case b
                      ((#x08)           ; BS
                       (flush-string)
                       (grv-text'backward-delete-char)
                       char-state)
                      ((#x0a)           ; LF
                       (flush-string)
                       (if overwrite?
                         (grv-text'move-cursor-next-line 1)
                         (grv-text'split-line))
                       char-state)
                      ((#x0d)           ; CR
                       (flush-string)
                       char-state)
                      ((#x1b)           ; ESC
                       (flush-string)
                       esc-state)
                      (else
                       (output-byte b)
                       char-state)))))
    (define esc-state
      (make-state (lambda (b)
                    (case b
                      ((#x5b)           ; [
                       (clear-params)
                       csi-state)
                      (else
                       char-state)))))
    (define csi-state
      (make-state (lambda (b)
                    (case b
                      ((#x41)           ; ESC [ n A : Cursor Up
                       (push-param)
                       (grv-text'move-cursor-up (get-param 0 1))
                       char-state)
                      ((#x42)           ; ESC [ n B : Cursor Down
                       (push-param)
                       (grv-text'move-cursor-down (get-param 0 1))
                       char-state)
                      ((#x43)           ; ESC [ n C : Cursor Forward
                       (push-param)
                       (grv-text'move-cursor-forward (get-param 0 1))
                       char-state)
                      ((#x44)           ; ESC [ n D : Cursor Back
                       (push-param)
                       (grv-text'move-cursor-back (get-param 0 1))
                       char-state)
                      ((#x45)           ; ESC [ n E : Cursor Next Line
                       (push-param)
                       (grv-text'move-cursor-next-line (get-param 0 1))
                       char-state)
                      ((#x46)       ; ESC [ n F : Cursor Previous Line
                       (push-param)
                       (grv-text'move-cursor-previous-line (get-param 0 1))
                       char-state)
                      ((#x47) ; ESC [ n G : Cursor Horizontal Absolute
                       (push-param)
                       (grv-text'move-cursor-horizontal-absolute (- (get-param 0 1) 1))
                       char-state)
                      ((#x48)          ; ESC [ n;m H : Cursor Position
                       (push-param)
                       (grv-text'move-cursor (- (get-param 1 1) 1) (- (get-param 0 1) 1))
                       char-state)
                      ((#x4a)           ; ESC [ n J : Erase in Display
                       (push-param)
                       (grv-text'erase-display (get-param 0 0))
                       char-state)
                      ((#x4b)           ; ESC [ n K : Erase in Line
                       (push-param)
                       (grv-text'erase-line (get-param 0 0))
                       char-state)
                      ((#x6d) ; ESC [ n(;n;...) m : Select Graphic Redition
                       (push-param)
                       (update-style)
                       char-state)
                      ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39) ; 0-9
                       (hold-param-data b)
                       csi-state)
                      ((#x3b)           ; ';'
                       (push-param)
                       csi-state)
                      (else
                       char-state)))))
    char-state))

(define (process-byte grv-text b overwrite?)
  (with-slots (processor-state) grv-text
    (processor-state b overwrite?)))


(define (process-text grv-text text overwrite?)
  (let ((gen (port->byte-generator (open-input-string text))))
    (let loop ((b (gen)))
      (process-byte grv-text b overwrite?)
      (unless (eof-object? b)
        (loop (gen))))))

(define (process-char grv-text c overwrite?)
  (process-text grv-text (string c) overwrite?))

(define (get-output-grv-text grv-text :optional (overwrite? #t))
  (let1 ctx (application-context)
    (make <virtual-output-port>
      :putb (cut process-byte grv-text <> overwrite?)
      :putc (cut process-char grv-text <> overwrite?)
      :puts (cut process-text grv-text <> overwrite?)
      :flush (lambda ()
               ;; flush can be called in the different application-context (e.g. it can be called by GC).
               (when (eq? (application-context) ctx)
                 (process-byte grv-text (eof-object) overwrite?)))
      :close (lambda ()
               ;; close can be called in the different application-context (e.g. it can be called by GC).
               (when (eq? (application-context) ctx)
                 (process-byte grv-text (eof-object) overwrite?))))))

(define (call-with-output-grv-text grv-text proc :key (overwrite? #t))
  (let1 out (get-output-grv-text grv-text overwrite?)
    (unwind-protect
        (proc out)
      (close-port out))))

(define (with-output-to-grv-text grv-text thunk :key (overwrite? #t))
  (let1 out (get-output-grv-text grv-text overwrite?)
    (unwind-protect
        (with-output-to-port (get-output-grv-text grv-text overwrite?) thunk)
      (close-port out))))

(define-jsobject-method <grv-text> insert-text (text)
  (process-text self text #f))

(define-jsobject-method <grv-text> print-text (text)
  (process-text self text #t))

(define-jsobject-method <grv-text> enable-key-input ()
  (set! (~ self'text-input-handler) (lambda (str)
                                      (self'insert-text str))))

(define-jsobject-method <grv-text> disable-key-input ()
  (set! (~ self'text-input-handler) #f))

(define-constant CALL-WITH-CLIPBOARD-TEXT 2)

(define-jsobject-method <grv-text> bind-key (key thunk :key (use-clipboard-text? #f))
  (with-slots (keymap) self
    (cond
      ((procedure? thunk)
       (hash-table-put! keymap key thunk)
       (self'set-key-event-availability key (logior (if use-clipboard-text? CALL-WITH-CLIPBOARD-TEXT 0))))
      ((not thunk)
       (hash-table-delete! keymap key)
       (self'set-key-event-availability key #f))
      (else
       (errorf "<procedure> or #f required, but got ~s" thunk)))))

(define-jsobject-method <grv-text> has-mark? ()
  (jslet/result ((self::object))
    (self.hasMark)))

;;;

(define (enable-screen-edit grv-text)
  (grv-text'show-cursor)
  (grv-text'clear-key-event-availability)
  (grv-text'set-move-cursor-by-mouse-click #t)

  (grv-text'bind-key "ArrowLeft" (lambda ()
                                   (grv-text'previous-char)))
  (grv-text'bind-key "S-ArrowLeft" (lambda ()
                                     (grv-text'previous-char 1 #t)))
  (grv-text'bind-key "ArrowRight" (lambda ()
                                    (grv-text'forward-char)))
  (grv-text'bind-key "S-ArrowRight" (lambda ()
                                      (grv-text'forward-char 1 #t)))
  (grv-text'bind-key "ArrowUp" (lambda ()
                                 (grv-text'previous-line)))
  (grv-text'bind-key "S-ArrowUp" (lambda ()
                                   (grv-text'previous-line 1 #t)))
  (grv-text'bind-key "ArrowDown" (lambda ()
                                   (grv-text'next-line)))
  (grv-text'bind-key "S-ArrowDown" (lambda ()
                                     (grv-text'next-line 1 #t)))
  (grv-text'bind-key "Backspace" (lambda ()
                                   (grv-text'backward-delete-char)))
  (grv-text'bind-key "Delete" (lambda ()
                                (grv-text'delete-char)))
  (grv-text'bind-key "Tab" (lambda ()
                             (grv-text'insert-text "\t")))
  (grv-text'bind-key "Home" (lambda ()
                              (grv-text'beginning-of-line)))
  (grv-text'bind-key "S-Home" (lambda ()
                                (grv-text'beginning-of-line #t)))
  (grv-text'bind-key "End" (lambda ()
                             (grv-text'end-of-line)))
  (grv-text'bind-key "S-End" (lambda ()
                               (grv-text'end-of-line #t)))
  (grv-text'bind-key "PageUp" (lambda ()
                                (grv-text'page-up)))
  (grv-text'bind-key "S-PageUp" (lambda ()
                                  (grv-text'page-up 1 #t)))
  (grv-text'bind-key "PageDown" (lambda ()
                                  (grv-text'page-down)))
  (grv-text'bind-key "S-PageDown" (lambda ()
                                    (grv-text'page-down 1 #t)))
  (set! (~ grv-text'text-input-handler) (lambda (str)
                                          (grv-text'insert-text str))))

(define (finish-line-edit grv-text reason)
  (let1 cont (~ grv-text'line-edit-cont)
    (unless cont
      (errorf "~s isn't in line editing" grv-text))
    (set! (~ grv-text'line-edit-cont) #f)
    (cont reason)))

(define (setup-default-line-edit-key-binding grv-text)
  (grv-text'push-key-event-availability)
  (grv-text'clear-key-event-availability)
  (grv-text'bind-key "ArrowLeft" (lambda ()
                                   (grv-text'previous-char)))
  (grv-text'bind-key "S-ArrowLeft" (lambda ()
                                     (grv-text'previous-char 1 #t)))
  (grv-text'bind-key "ArrowRight" (lambda ()
                                    (grv-text'forward-char)))
  (grv-text'bind-key "S-ArrowRight" (lambda ()
                                      (grv-text'forward-char 1 #t)))
  (grv-text'bind-key "Backspace" (lambda ()
                                   (grv-text'backward-delete-char)))
  (grv-text'bind-key "Delete" (lambda ()
                                (grv-text'delete-char)))
  (grv-text'bind-key "Tab" (lambda ()
                             (grv-text'insert-text "\t")))
  (grv-text'bind-key "Home" (lambda ()
                              (grv-text'beginning-of-line)))
  (grv-text'bind-key "S-Home" (lambda ()
                                (grv-text'beginning-of-line #t)))
  (grv-text'bind-key "End" (lambda ()
                             (grv-text'end-of-line)))
  (grv-text'bind-key "S-End" (lambda ()
                               (grv-text'end-of-line #t)))
  (grv-text'bind-key "Enter" (lambda ()
                               (newline-or-commit grv-text)))
  (grv-text'bind-key "Escape" (lambda ()
                                (cancel-edit grv-text))))

(define (start-line-edit grv-text prompt)
  (define (%process-queued-input)
    (let1 str (dequeue-input grv-text)
      (cond
        ((not str)
         (%start-line-edit))
        ((equal? str "\n")
         (set! (~ grv-text'line-edit-cont) values)
         (newline-or-commit grv-text))
        (else
         (%grv-text'insert-text str)
         (process-queued-input)))))

  (define (%start-line-edit)
    (shift-callback callback
      (set! (~ grv-text'line-edit-cont) callback)))

  (grv-text'print-text prompt)
  (set-start-column! grv-text (grv-text'cursor-row) (grv-text'cursor-column))
  (%process-queued-input))

;; (define (%read-line/edit grv-text :key (prompt "") (setup #f))
;;   (define (get-line)
;;     (let* ((row (grv-text'cursor-row))
;;            (line (grv-text'get-line row)))
;;       (begin0
;;           (substring line (get-start-column grv-text row) (string-length line))
;;         (set-start-column! grv-text row #f))))

;;   (define (process-queued-input)
;;     (let1 str (dequeue-input grv-text)
;;       (cond
;;         ((not str)
;;          (start-line-edit))
;;         ((equal? str "\n")
;;          (values (get-line) 'commit))
;;         (else
;;          (grv-text'insert-text str)
;;          (process-queued-input)))))

;;   (define (start-line-edit)
;;     (setup-default-line-edit-key-binding grv-text)
;;     (when setup
;;       (setup))
;;     (set! (~ grv-text'text-input-handler) (lambda (input-str)
;;                                             (match-let1 (str rest ...) (string-split input-str "\n")
;;                                               (grv-text'insert-text str)
;;                                               (unless (null? rest)
;;                                                 (for-each (cut enqueue-input grv-text <>) (intersperse "\n" rest))
;;                                                 (finish-line-edit grv-text 'commit)))))
;;     (let* ((reason (shift-callback callback
;;                      (set! (~ grv-text'line-edit-cont) callback)))
;;            (line (get-line)))
;;       (grv-text'pop-key-event-availability)
;;       (values line reason)))

;;   (grv-text'set-move-cursor-by-mouse-click #f)
;;   (grv-text'print-text prompt)
;;   (set-start-column! grv-text (grv-text'cursor-row) (grv-text'cursor-column))
;;   (grv-text'focus)
;;   (process-queued-input))

(define (get-input-line grv-text row)
  (hash-table-get (~ grv-text'input-line-table) row ""))

(define (get-input-text grv-text)
  (apply string-append
    (intersperse "\n"
                 (map (lambda (row)
                        (get-input-line grv-text row))
                      (iota (+ (- (~ grv-text'end-row) (~ grv-text'start-row)) 1) (~ grv-text'start-row))))))

(define (newline-or-commit grv-text)
  (finish-line-edit grv-text 'newline-or-commit))

(define (cancel-edit grv-text)
  (finish-line-edit grv-text 'cancel))

;; (define (previous-line grv-text :optional (shift-mark? #f))
;;   (let ((col (grv-text'cursor-column))
;;         (row (grv-text'cursor-row)))
;;     (cond
;;       ((<= (~ grv-text'start-row) (- row 1) (~ grv-text'end-row))
;;        (grv-text'previous-line 1 shift-mark?)
;;        (let1 start-col (get-start-column grv-text (- row 1))
;;          (when (< col start-col)
;;            (grv-text'move-cursor-horizontal-absolute start-col))))
;;       ((< (- row 1) (~ grv-text'start-row))
;;        ;; TODO: Implement history
;;        #f)
;;       (else
;;        #f))))

;; (define (next-line grv-text :optional (shift-mark? #f))
;;   (let ((col (grv-text'cursor-column))
;;         (row (grv-text'cursor-row)))
;;     (when (<= (~ grv-text'start-row) (+ row 1) (~ grv-text'end-row))
;;       (grv-text'next-line 1 shift-mark?)
;;       (let1 start-col (get-start-column grv-text (+ row 1))
;;         (when (< col start-col)
;;           (grv-text'move-cursor-horizontal-absolute start-col))))))

;; (define (previous-char grv-text :optional (shift-mark? #f))
;;   (let ((col (grv-text'cursor-column))
;;         (row (grv-text'cursor-row)))
;;     (cond
;;       ((< (get-start-column grv-text row) col)
;;        (grv-text'previous-char 1 shift-mark?))
;;       ((< (~ grv-text'start-row) row)
;;        (grv-text'previous-line 1 shift-mark?)
;;        (grv-text'end-of-line shift-mark?))
;;       (else
;;        #f))))

;; (define (forward-char grv-text :optional (shift-mark? #f))
;;   (let ((col (grv-text'cursor-column))
;;         (row (grv-text'cursor-row)))
;;     (cond
;;       ((< col (string-length (grv-text'get-line row)))
;;        (grv-text'previous-char 1 shift-mark?))
;;       ((< (~ grv-text'start-row) row)
;;        (grv-text'previous-line 1 shift-mark?)
;;        (grv-text'end-of-line shift-mark?))
;;       (else
;;        #f))))

(define (read-string-from-grv-text grv-text :key (prompt "") (setup #f) (input-continues #f))
  (define (get-line row)
    (let1 line (grv-text'get-line row)
      (substring line (get-start-column grv-text row) (string-length line))))

  (define (get-prompt)
    (cond
      ((string? prompt)
       prompt)
      ((procedure? prompt)
       (prompt (- (grv-text'cursor-row) (~ grv-text'start-row))))))

  (define (line-edit-loop)
    (case (start-line-edit grv-text (get-prompt))
      ((newline-or-commit)
       (let* ((row (grv-text'cursor-row))
              (line (get-line row)))
         (hash-table-put! (~ grv-text'input-line-table) row line))
       (let1 text (get-input-text grv-text)
         (cond
           ((and (procedure? input-continues)
                 (input-continues text))
            ;; newline
            (grv-text'print-text "\n")
            (set! (~ grv-text'end-row) (max (~ grv-text'end-row) (grv-text'cursor-row)))
            (line-edit-loop))
           (else
            ;; commit
            text))))
      ((cancel)
       #f)))

  (unless (or (eq? input-continues #f)
              (procedure? input-continues))
    (errorf "<procedure> or #f required, but got ~s" input-continues))
  (unless (or (string? prompt)
              (procedure? prompt))
    (errorf "<string> or <procedure> required, but got ~s" prompt))

  ;; TODO: Implement input-continues for multi-line edit.
  (let1 row (grv-text'cursor-row)
    (set! (~ grv-text'start-row) row)
    (set! (~ grv-text'end-row) row))
  (hash-table-clear! (~ grv-text'input-line-table))
  (hash-table-clear! (~ grv-text'start-column-table))
  (set! (~ grv-text'text-input-handler) (lambda (input-str)
                                          (match-let1 (str rest ...) (string-split input-str "\n")
                                            (grv-text'insert-text str)
                                            (unless (null? rest)
                                              (for-each (cut enqueue-input grv-text <>) (intersperse "\n" rest))
                                              (newline-or-commit grv-text)))))

  (setup-default-line-edit-key-binding grv-text)
  (when setup
    (setup))

  (grv-text'focus)

  (unwind-protect
      (line-edit-loop)
    (hash-table-clear! (~ grv-text'input-line-table))
    (hash-table-clear! (~ grv-text'start-column-table))
    (set! (~ grv-text'start-row) #f)
    (set! (~ grv-text'end-row) #f)))

