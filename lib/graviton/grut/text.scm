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
  (use gauche.record)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use graviton.app)
  (use graviton.async)
  (use graviton.browser-objects)
  (use graviton.comm)
  (use graviton.grut.audio)
  (use graviton.grut.clipboard)
  (use graviton.jsffi)
  (use graviton.misc)
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use srfi-42)
  (use srfi-130)
  (use text.console)
  (use text.html-lite)
  (use util.match)

  (export html:grv-text
          clipboard-text
          <input-context>
          <grv-text>

          get-text-input-port

          call-with-console
          putch
          putstr
          beep
          getch
          chready?
          query-cursor-position
          move-cursor-to
          clear-screen
          clear-to-eol
          clear-to-eos
          hide-cursor
          show-cursor
          cursor-down/scroll-up
          cursor-up/scroll-down
          query-screen-size
          set-character-attribute
          reset-character-attribute
          with-character-attribute

          make-keymap
          global-keymap
          bind-key

          edit:backward-delete-char
          edit:beginning-of-edit-area
          edit:beginning-of-line
          edit:cancel-edit
          edit:copy
          edit:cut
          edit:delete-char
          edit:end-of-edit-area
          edit:end-of-line
          edit:forward-char
          edit:insert-string
          edit:newline-or-commit
          edit:next-line
          edit:page-up
          edit:page-down
          edit:paste
          edit:previous-char
          edit:previous-line
          read-text/edit))

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

(define-class <input-context> ()
  ((text-element :init-keyword :text-element)
   (edit-cont :init-value #f)
   (lines :init-form (make-vector 1 ""))
   (version :init-value 0)
   (offset :init-value 0)
   (start-row :init-value #f)
   (end-row :init-value #f)
   (%cursor-column :init-value 0)
   (cursor-column :allocation :virtual
                  :slot-ref (lambda (obj)
                              (min (slot-ref obj '%cursor-column)
                                   (+ (slot-ref obj 'offset)
                                      (string-length (get-input-line obj (slot-ref obj 'cursor-row))))))
                  :slot-set! (lambda (obj col)
                               (slot-set! obj '%cursor-column col)))
   (cursor-row :init-keyword :row)
   (input-continues :init-keyword :input-continues)
   (prompter :init-keyword :prompter)
   (force-mark? :init-value #f)
   (keymap :init-keyword :keymap)
   (this-key :init-value #f)
   (clipboard-text :init-value #f)))

(define-method initialize ((ctx <input-context>) initargs)
  (next-method)

  (let1 row (slot-ref ctx 'cursor-row)
    (slot-set! ctx 'start-row row)
    (slot-set! ctx 'end-row row)))

(define (modifier-key-pressed? input-context modifier)
  (with-slots (this-key) input-context
    (cond
      (this-key
       (let1 mods (drop-right (string-split this-key "-") 1)
         (not (not (member modifier mods)))))
      (else
       #f))))

(define shift-pressed? (cut modifier-key-pressed? <> "S"))
(define meta-pressed? (cut modifier-key-pressed? <> "M"))
(define alt-pressed? (cut modifier-key-pressed? <> "A"))
(define control-pressed? (cut modifier-key-pressed? <> "C"))

(define (clipboard-text input-context)
  (~ input-context'clipboard-text))

(define-class <keymap> ()
  ((parent :init-keyword :parent)
   (key-table :init-form (make-hash-table 'equal?))
   (text-element-list :init-value '())))

(define (make-keymap :optional (parent #f))
  (make <keymap> :parent parent))

(define (bind-key map key action :key (use-clipboard-text? #f))
  (with-slots (key-table text-element-list) map
    (cond
      (action
       (let ((proc (cond
                     ((procedure? action)
                      action)
                     ((string? action)
                      (lambda (input-context)
                        (edit:insert-string input-context action)))
                     (else
                      (errorf "<procedure> or <string> required, but got ~s" action))))
             (arg (logior (if use-clipboard-text? CALL-WITH-CLIPBOARD-TEXT 0))))
         (hash-table-put! key-table key (list proc arg))
         (for-each (lambda (text-element)
                     (text-element'set-key-event-availability key arg))
                   text-element-list)))
      (else
       (hash-table-delete! key-table key)
       (for-each (lambda (text-element)
                   (text-element'set-key-event-availability key #f))
                 text-element-list)))))

(define (switch-keymap input-context keymap)
  (with-slots (text-element) input-context
    (define (%bind-keys keymap)
      (with-slots (parent key-table) keymap
        (when parent
          (%bind-keys parent))
        (hash-table-for-each key-table (lambda (key proc+arg)
                                         (match-let1 (proc arg) proc+arg
                                           (text-element'set-key-event-availability key arg))))))

    (text-element'clear-key-event-availability)
    (cond
      (keymap
       (with-slots (text-element-list) keymap
         (%bind-keys keymap)
         (push! text-element-list text-element)))
      (else
       (with-slots (keymap) input-context
         (with-slots (text-element-list) keymap
           (set! text-element-list (delete text-element text-element-list))
           (set! keymap #f)))))))

(define (find-key-proc keymap key)
  (with-slots (parent key-table) keymap
    (or (car (hash-table-get key-table key '(#f #f)))
        (and parent (find-key-proc parent key)))))

(define global-keymap (make-window-parameter*
                        (lambda ()
                          (rlet1 keymap (make-keymap)
                            ;; TODO: Check user-agent here
                            (for-each (match-lambda
                                        ((key proc . args)
                                         (apply bind-key keymap key proc args)))
                                      `(("ArrowLeft" ,edit:previous-char)
                                        ("S-ArrowLeft" ,edit:previous-char)
                                        ("ArrowRight" ,edit:forward-char)
                                        ("S-ArrowRight" ,edit:forward-char)
                                        ("ArrowUp" ,edit:previous-line)
                                        ("S-ArrowUp" ,edit:previous-line)
                                        ("ArrowDown" ,edit:next-line)
                                        ("S-ArrowDown" ,edit:next-line)
                                        ("PageUp" ,edit:page-up)
                                        ("S-PageUp" ,edit:page-up)
                                        ("PageDown" ,edit:page-down)
                                        ("S-PageDown" ,edit:page-down)
                                        ("Backspace" ,edit:backward-delete-char)
                                        ("Delete" ,edit:delete-char)
                                        ("Tab" "\t")
                                        ("Home" ,edit:beginning-of-line)
                                        ("S-Home" ,edit:beginning-of-line)
                                        ("End" ,edit:end-of-line)
                                        ("S-End" ,edit:end-of-line)
                                        ("Enter" ,edit:newline-or-commit)))))))

(define-record-type <text-mark>
  make-text-mark text-mark?
  (start-column text-mark-start-column text-mark-start-column-set!)
  (start-row text-mark-start-row text-mark-start-row-set!)
  (end-column text-mark-end-column text-mark-end-column-set!)
  (end-row text-mark-end-row text-mark-end-row-set!))

(define-class <grv-text> (<html-element> <virtual-output-port>)
  ((text-content :jsproperty "textContent")
   (page-size :jsproperty "pageSize")
   (number-of-lines :jsproperty "row")
   (bell-style :jsproperty "bellStyle")
   (processor-state :init-value #f)
   (input-hook :init-form (make-hook 2))
   (clipboard-hook :init-form (make-hook 1))
   (mouse-hook :init-form (make-hook 3))
   (input-queue :init-form (make-mtqueue))
   (pending-characters :init-value '())
   (text-mark :init-value #f))
  :jsclass "GrvText")

(define-method initialize ((self <grv-text>) initargs)
  (next-method)
  (self'set-callbacks
   ;; text-input
   (lambda (str)
     (with-slots (input-queue) self
       (enqueue-input self 'text str)
       (run-hook (~ self'input-hook) 'text str)))
   ;; key
   (lambda (key clipboard)
     (with-slots (input-queue) self
       (enqueue-input self 'key key clipboard)
       (run-hook (~ self'input-hook) 'key key)))
   ;; clipboard
   (lambda (code)
     (run-hook (~ self'clipboard-hook) (assoc-ref '((1 . cut) (2 . copy) (3 . paste)) code)))
   ;; mouse
   (let ((down-col #f)
         (down-row #f))
     (lambda (event-name col row)
       (with-slots (text-mark) self
         (cond
           ((equal? event-name "Mouse-Down-0")
            (set! down-col col)
            (set! down-row row)
            (clear-mark! self))
           ((equal? event-name "S-Mouse-Down-0")
            (set! down-col col)
            (set! down-row row)
            (cond
              (text-mark
               (set-mark! self col row))
              (else
               (set-mark! self col row col row))))
           ((and (equal? event-name "Mouse-Drag-0")
                 down-col
                 down-row)
            (cond
              (text-mark
               (set-mark! self col row))
              (else
               (set-mark! self down-col down-row col row))))))
       (run-hook (~ self'mouse-hook) event-name col row))))
  (slot-set! self 'processor-state (make-text-processor self))

  ;; slots of <virtual-output-port>
  (let1 ctx (application-context)
    (slot-set! self 'putb (cut process-byte self <>))
    (slot-set! self 'putc (cut process-char self <>))
    (slot-set! self 'puts (cut process-text self <>))
    (slot-set! self 'flush (lambda ()
                             ;; flush can be called in the different application-context (e.g. it can be called by GC).
                             (when (eq? (application-context) ctx)
                               (process-byte self (eof-object)))))
    (slot-set! self 'close (lambda ()
                             ;; close can be called in the different application-context (e.g. it can be called by GC).
                             (when (eq? (application-context) ctx)
                               (process-byte self (eof-object)))))))

(define-automatic-jsobject-methods <grv-text>
  "setCallbacks"
  "setKeyEventAvailability"
  "clearKeyEventAvailability"
  "saveKeyEventAvailability"
  "restoreKeyEventAvailability"
  "setMoveCursorByMouseClick"

  ("queryScreenSize" :result #t)

  "setTextAttribute"
  "saveTextAttribute"
  "restoreTextAttribute"
  "updateTextAttribute"
  "updateLineAttribute"

  "writeLine"
  "updateLineString"
  "eraseDisplay"
  "eraseLine"
  "insertLine"
  "removeLine"
  "removeAllLines"
  "setStartColumn"
  "visualBell"

  "moveCursor"
  "moveCursorUp"
  "moveCursorDown"
  "moveCursorForward"
  "moveCursorBack"
  "moveCursorNextLine"
  "moveCursorPreviousLine"
  "moveCursorHorizontalAbsolute"

  "scrollUp"
  "scrollDown"

  ("extractMarkRegionText" :result #t)

  "setMark"
  "clearMark"
  )

(define-jsobject-method <grv-text> cursor-column ()
  (jslet/result ((self::object))
    (result self.cursor.column)))

(define-jsobject-method <grv-text> cursor-row ()
  (jslet/result ((self::object))
    (result self.cursor.row)))

(define-jsobject-method <grv-text> cursor-position ()
  (jslet/result ((self::object))
    (result self.cursor.column self.cursor.row)))

(define-jsobject-method <grv-text> set-cursor-position! (col row)
  (jslet ((self::object)
          (col)
          (row))
    (set! self.cursor.column col)
    (set! self.cursor.row row)))

(define-jsobject-method <grv-text> show-cursor ()
  (jslet ((self::object))
    (self.cursor.show)))

(define-jsobject-method <grv-text> hide-cursor ()
  (jslet ((self::object))
    (self.cursor.hide)))

(define-jsobject-method <grv-text> cursor-visible? ()
  (jslet/result ((self::object))
    (result self.cursor.visible)))

(define (enqueue-input grv-text type :rest args)
  (with-slots (input-queue) grv-text
    (case type
      ((text)
       (for-each (lambda (str)
                   (for-each (cut enqueue! input-queue <>)
                             (map (cut list 'text <>) (intersperse "\n" (string-split str "\n")))))
                 args))
      ((key)
       (enqueue! input-queue (cons type args))))))

(define (dequeue-input grv-text :optional (wait? #f))
  (with-slots (input-queue) grv-text
    (if wait?
      (dequeue/wait! input-queue)
      (dequeue! input-queue #f))))

(define (clear-input-buffer! grv-text)
  (with-slots (input-queue) grv-text
    (dequeue-all! input-queue))
  (undefined))

(define (get-text-input-port grv-text :key (echo? #t))
  (with-slots (input-queue) grv-text
    (let1 in #f
      (make <virtual-input-port>
        :getb (lambda ()
                (let loop ((b (if in
                                (read-byte in)
                                (eof-object))))
                  (cond
                    ((eof-object? b)
                     (set! in (open-input-string
                                (rlet1 str (match (dequeue/wait! input-queue)
                                            (('text str)
                                             str)
                                            (('key key clipboard)
                                             (if (#/C-[a-zA-Z]/ key)
                                               (string (ucs->char (- (char->ucs (first
                                                                                  (string->list
                                                                                    (string-downcase
                                                                                      (string-take-right key 1)))))
                                                                     #x60)))
                                               "")))
                                  (when echo?
                                    (putstr grv-text str)
                                    (flush-client-request)))))
                     (loop (read-byte in)))
                    (else
                     b))))))))

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
  (let ((string-port #f)
        (data '())
        (params '())
        (intensity? #f))

    (define (output-byte b)
      (unless string-port
        (set! string-port (open-output-string :name (format "(~s text-processor)" grv-text))))
      (write-byte b string-port))

    (define (flush-string)
      (when string-port
        (grv-text'write-line (get-output-string string-port)))
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
      (let loop ((params (reverse params))
                 (updaters '()))
        (cond
          ((null? params)
           (for-each (lambda (updater)
                       (updater))
                     updaters))
          (else
           (case (car params)
             ((0 #f)                    ; Reset/Normal
              (set! intensity? #f)
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "color" #f)
                            (grv-text'update-text-attribute "background-color" #f)
                            (grv-text'update-text-attribute "font-style" #f)
                            (grv-text'update-text-attribute "text-decoration" #f)
                            (grv-text'update-text-attribute "filter" #f)
                            (grv-text'update-text-attribute "background" #f))
                          updaters)))
             ((1)                       ; Increased intensity
              (set! intensity? #t)
              (loop (cdr params) updaters))
             ((2)                       ; Decreased intensity
              (set! intensity? #f)
              (loop (cdr params) updaters))
             ((3)                       ; Italic
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "font-style" "italic"))
                          updaters)))
             ((4)                       ; Underline
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "text-decoration" "underline"))
                          updaters)))
             ((7)                       ; Reverse video
              (loop (cdr params)
                    (cons (lambda ()
                            ;; This style is different from "Reverse video". But there is no easy way to implement it in CSS.
                            (grv-text'update-text-attribute "filter" "invert(1)")
                            ;; background-color style can't be used here because TextAttribute.updateSpan will update
                            ;; --grv-text-edit-background-color var.
                            (grv-text'update-text-attribute "background" "var(--grv-text-edit-background-color)"))
                          updaters)))
             ((22)                      ; Normal color or intensity
              (set! intensity? #f)
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "color" #f)
                            (grv-text'update-text-attribute "background-color" #f)
                            (grv-text'update-text-attribute "filter" #f)
                            (grv-text'update-text-attribute "background" #f))
                          updaters)))
             ((23)                      ; Not italic
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "font-style" #f))
                          updaters)))
             ((24)                      ; Underline off
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "text-decoration" #f))
                          updaters)))
             ((27)                      ; Reverse off
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "filter" #f)
                            (grv-text'update-text-attribute "background" #f))
                          updaters)))
             ((30 31 32 33 34 35 36 37) ; Set foreground color
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "color"
                                                            (parse-basic-color (+ (- (car params) 30) (if intensity? 8 0)))))
                          updaters)))
             ((38)                ; Set foreground color (8 or 24 bit)
              (receive (color rest) (parse-color (cdr params))
                (loop rest
                      (cons (lambda ()
                              (grv-text'update-text-attribute "color" color))
                            updaters))))
             ((39)                      ; Default foreground color
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "color" #f))
                          updaters)))
             ((40 41 42 43 44 45 46 47) ; Set background color
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "background-color" (parse-basic-color (- (car params) 40))))
                          updaters)))
             ((48)                ; Set background color (8 or 24 bit)
              (receive (color rest) (parse-color (cdr params))
                (loop rest
                      (cons (lambda ()
                              (grv-text'update-text-attribute "background-color" color))
                            updaters))))
             ((49)                      ; Default background color
              (loop (cdr params)
                    (cons (lambda ()
                            (grv-text'update-text-attribute "background-color" #f))
                          updaters)))
             (else
              (loop (cdr params) updaters)))))))

    (define (make-state proc)
      (letrec ((state (lambda (b)
                        (slot-set! grv-text 'processor-state (cond
                                                               ((eof-object? b)
                                                                (flush-string)
                                                                state)
                                                               (else
                                                                (proc b)))))))
        state))

    (define char-state
      (make-state (lambda (b)
                    (case b
                      ((#x08)           ; BS
                       (flush-string)
                       (grv-text'move-cursor-back 1)
                       (grv-text'write-line " " #t)
                       (grv-text'move-cursor-back 1)
                       char-state)
                      ((#x0a)           ; LF
                       (flush-string)
                       (grv-text'move-cursor-next-line 1)
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

(define (process-byte grv-text b)
  (with-slots (processor-state) grv-text
    (processor-state b)))

(define (process-text grv-text text)
  (let ((gen (port->byte-generator (open-input-string text))))
    (let loop ((b (gen)))
      (process-byte grv-text b)
      (unless (eof-object? b)
        (loop (gen))))))

(define (process-char grv-text c)
  (process-text grv-text (string c)))

(define-jsobject-method <grv-text> print-text (text)
  (process-text self text))

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

;;;

(define special-key-alist
  '(("Tab" . #\x09)
    ("Enter" . #\x0a)
    ("Space" . #\x20)
    ("ArrowUp" . KEY_UP)
    ("ArrowDown" . KEY_DOWN)
    ("ArrowLeft" . KEY_LEFT)
    ("ArrowRight" . KEY_RIGHT)
    ("Home" . KEY_HOME)
    ("End" . KEY_END)
    ("Insert" . KEY_INS)
    ("Delete" . #\x7f)
    ("PageDown" . KEY_PGDN)
    ("PageUp" . KEY_PGUP)
    ("F1" . KEY_F1)
    ("F2" . KEY_F2)
    ("F3" . KEY_F3)
    ("F4" . KEY_F4)
    ("F5" . KEY_F5)
    ("F6" . KEY_F6)
    ("F7" . KEY_F7)
    ("F8" . KEY_F8)
    ("F9" . KEY_F9)
    ("F10" . KEY_F10)
    ("F11" . KEY_F11)
    ("F12" . KEY_F12)
    ("Escape" . #\x1b)))

(define (init-console-keys grv-text mode)
  (let* ((special-keys (map car special-key-alist))
         (ctrls (map (^x (string-append "C-" x)) (map string (char-set->list #[a-zA-Z]))))
         (alts (map (^x (string-append "A-" x)) (append (map string (char-set->list #[!-~]))
                                                        ctrls
                                                        special-keys))))
    (grv-text'clear-key-event-availability)
    (for-each (lambda (key)
                (grv-text'set-key-event-availability key 0))
              (case mode
                ((raw)
                 (append special-keys ctrls alts))
                ((rare cooked)
                 special-keys)))))

(define-method call-with-console ((grv-text <grv-text>) proc :key (mode 'rare))
  (grv-text'save-key-event-availability)
  (grv-text'focus)
  (unwind-protect
      (begin
        (init-console-keys grv-text mode)
        (proc grv-text))
    (grv-text'restore-key-event-availability)))

(define-method putch ((grv-text <grv-text>) c)
  (grv-text'print-text (string c)))

(define-method putstr ((grv-text <grv-text>) str)
  (grv-text'print-text str))

(define-method beep ((grv-text <grv-text>))
  (let-values (((audio? visual?) (apply values (assoc-ref '(("all" #t #t)
                                                            ("audible" #t #f)
                                                            ("visual" #f #t)
                                                            ("none" #f #f))
                                                          (~ grv-text'bell-style)
                                                          '(#f #f)))))
    (when audio?
      (play-beep 1000 0.1))
    (when visual?
      (grv-text'visual-bell))))

(define (%getch grv-text wait?)
  (define (key->ch key)
    (match-let1 (stem . modifiers) (reverse (string-split key "-"))
      (let loop ((stem stem)
                 (modifiers modifiers))
        (cond
          ((and (null? modifiers)
                (= (string-length stem) 1))
           (first (string->list stem)))
          ((member "A" modifiers)
           (and-let1 ch (loop stem (delete "A" modifiers))
             `(ALT ,ch)))
          ((and (member "C" modifiers)
                (#/[a-z]/i stem))
           (ucs->char (- (char->ucs (first (string->list (string-downcase stem)))) #x60)))
          ((assoc-ref special-key-alist key #f)
           => values)
          (else
           #f)))))

  (with-slots (pending-characters) grv-text
    (cond
      ((not (null? pending-characters))
       (rlet1 c (car pending-characters)
         (set! pending-characters (cdr pending-characters))))
      (else
       (or (match (dequeue-input grv-text wait?)
             (('key key clipboard)
              (key->ch key))
             (('text str)
              (match-let1 (ch . rest) (reverse (string->list str))
                (set! pending-characters rest)
                ch))
             (_
              #f))
           (and wait?
                (%getch grv-text wait?)))))))

(define-method getch ((grv-text <grv-text>))
  (%getch grv-text #t))

(define-method chready? ((grv-text <grv-text>))
  (with-slots (pending-characters) grv-text
    (let1 ch (%getch grv-text #f)
      (cond
        (ch
         (push! pending-characters ch)
         #t)
        (else
         #f)))))

(define-method query-cursor-position ((grv-text <grv-text>))
  (grv-text'cursor-position))

(define-method move-cursor-to ((grv-text <grv-text>) row column)
  (flush grv-text)
  (grv-text'move-cursor column row))

(define-method reset-terminal ((grv-text <grv-text>))
  (clear-screen grv-text)
  (reset-character-attribute grv-text))

(define-method clear-screen ((grv-text <grv-text>))
  (flush grv-text)
  (grv-text'move-cursor 0 0)
  (grv-text'erase-display 2))

(define-method clear-to-eol ((grv-text <grv-text>))
  (flush grv-text)
  (grv-text'erase-line 0))

(define-method clear-to-eos ((grv-text <grv-text>))
  (flush grv-text)
  (grv-text'erase-display 0))

(define-method hide-cursor ((grv-text <grv-text>))
  (flush grv-text)
  (grv-text'hide-cursor))

(define-method show-cursor ((grv-text <grv-text>))
  (flush grv-text)
  (grv-text'show-cursor))

(define-method query-screen-size ((grv-text <grv-text>))
  (match-let1 #(w h) (grv-text'query-screen-size)
    (values w h)))

(define-method cursor-down/scroll-up ((grv-text <grv-text>))
  (let-values (((w h) (query-screen-size grv-text))
               ((col row) (query-cursor-position grv-text)))
    (cond
      ((< (+ row 1) h)
       (move-cursor-to (+ row 1) col))
      (else
       (move-cursor-to (+ row 1) col)
       (dotimes (_ (max 0 (- (~ grv-text'number-of-lines) h)))
         (grv-text'remove-line 0))))))

(define-method cursor-up/scroll-down ((grv-text <grv-text>))
  (let-values (((w h) (query-screen-size grv-text))
               ((col row) (query-cursor-position grv-text)))
    (cond
      ((< 0 row)
       (move-cursor-to (- row 1) col))
      (else
       (grv-text'insert-line 0)
       (move-cursor-to 0 col)
       (dotimes (_ (max 0 (- (~ grv-text'number-of-lines) h)))
         (grv-text'remove-line h))))))

(define-method set-character-attribute ((grv-text <grv-text>) spec)
  (define (color->code col bg?)
    (or (and-let1 code (assq-ref '((black . 30)
                                   (red . 31)
                                   (green . 32)
                                   (yellow . 33)
                                   (blue . 34)
                                   (magenta . 35)
                                   (cyan . 36)
                                   (white . 37))
                                 col
                                 #f)
          (if bg?
            (+ 10 code)
            code))
        0))
  (define (option->code opt)
    (assq-ref '((bright . 1)
                (reverse . 7)
                (underscore . 4))
              opt
              0))
  (let-optionals* spec
      ((fgcolor #f)
       (bgcolor #f)
       . opts)
    (format grv-text
            "\x1b[~am"
            (string-join (map number->string
                              (delete 0 (list* (color->code fgcolor #f) (color->code bgcolor #t) (map option->code opts))))
                         ";"))))

(define-method reset-character-attribute ((grv-text <grv-text>))
  (format grv-text "\x1b[m"))

(define-method with-character-attribute ((grv-text <grv-text>) attrs thunk)
  (flush grv-text)
  (grv-text'save-text-attribute)
  (let-values (((col row) (query-cursor-position grv-text)))
    (let1 visible? (grv-text'cursor-visible?)
      (unwind-protect
          (begin
            (set-character-attribute grv-text attrs)
            (thunk))
        (if visible?
          (show-cursor grv-text)
          (hide-cursor grv-text))
        (move-cursor-to grv-text row col)
        (grv-text'restore-text-attribute)))))

;;;

(define-method set-mark! ((grv-text <grv-text>) start-col start-row end-col end-row)
  (with-slots (text-mark) grv-text
    (let1 mark (make-text-mark start-col start-row end-col end-row)
      (set! text-mark mark)
      (grv-text'set-mark start-col start-row end-col end-row))))

(define-method set-mark! ((grv-text <grv-text>) col row)
  (with-slots (text-mark) grv-text
    (cond
      (text-mark
       (text-mark-end-column-set! text-mark col)
       (text-mark-end-row-set! text-mark row))
      (else
       (set! text-mark (make-text-mark col row col row))))
    (grv-text'set-mark (text-mark-start-column text-mark)
                       (text-mark-start-row text-mark)
                       (text-mark-end-column text-mark)
                       (text-mark-end-row text-mark))))

(define-method set-mark! ((input-context <input-context>) col row)
  (with-slots (text-element) input-context
    (set-mark! text-element col row)))

(define-method set-mark! ((input-context <input-context>))
  (with-slots (cursor-column cursor-row) input-context
    (set-mark! input-context cursor-column cursor-row)))

(define-method clear-mark! ((grv-text <grv-text>))
  (with-slots (text-mark) grv-text
    (set! text-mark #f)
    (grv-text'clear-mark)))

(define-method clear-mark! ((input-context <input-context>))
  (with-slots (text-element) input-context
    (clear-mark! text-element)))

(define (edit:toggle-mark! input-context)
  (with-slots (text-element cursor-column cursor-row force-mark?) input-context
    (with-slots (text-mark) text-element
      (cond
        (text-mark
         (clear-mark! text-element)
         (set! force-mark? #f))
        (else
         (set-mark! text-element cursor-column cursor-row cursor-column cursor-row)
         (set! force-mark? #t))))))

;;;

(define (finish-edit input-context reason)
  (with-slots (edit-cont) input-context
    (edit-cont (get-input-content input-context) reason)))

(define (process-queued-input input-context)
  (with-slots (text-element cursor-column cursor-row offset keymap this-key clipboard-text) input-context
    (let1 input (dequeue-input text-element)
      (match input
        (#f
         #f)
        (('text "\n")
         (edit:newline-or-commit input-context)
         (process-queued-input input-context))
        (('text str)
         (delete-mark-region input-context)
         (let* ((line (get-input-line input-context cursor-row))
                (cur (- cursor-column offset))
                (head (string-take line cur))
                (tail (string-drop line cur))
                (updated-line (string-append head str tail)))
           (set-input-line! input-context cursor-row updated-line)
           (text-element'update-line-string cursor-row offset -1 updated-line)
           (inc! cursor-column (string-length str))
           (text-element'move-cursor cursor-column cursor-row))
         (process-queued-input input-context))
        (('key key clipboard)
         (and-let1 proc (find-key-proc keymap key)
           (dynamic-wind
               (lambda ()
                 (set! this-key key)
                 (set! clipboard-text clipboard))
               (lambda ()
                 (proc input-context))
               (lambda ()
                 (set! this-key #f)
                 (set! clipboard-text #f))))
         (process-queued-input input-context))))))

(define (get-input-line input-context row)
  (with-slots (lines start-row) input-context
    (vector-ref lines (- row start-row))))

(define (set-input-line! input-context row line)
  (with-slots (lines start-row version) input-context
    (vector-set! lines (- row start-row) line)
    (inc! version)))

(define (insert-input-line! input-context row line)
  (with-slots (text-element lines start-row end-row version) input-context
    (let* ((len (vector-length lines))
           (new-lines (make-vector (+ len 1)))
           (i (- row start-row)))
      (vector-copy! new-lines 0 lines 0 i)
      (vector-set! new-lines i line)
      (vector-copy! new-lines (+ i 1) lines i len)
      (set! lines new-lines)
      (inc! end-row)
      (text-element'insert-line row))
    (inc! version)))

(define (delete-input-line! input-context row)
  (with-slots (text-element lines start-row end-row version) input-context
    (let* ((len (vector-length lines))
           (new-lines (make-vector (- len 1)))
           (i (- row start-row)))
      (vector-copy! new-lines 0 lines 0 i)
      (when (< (+ i 1) len)
        (vector-copy! new-lines i lines (+ i 1) len))
      (set! lines new-lines)
      (dec! end-row)
      (text-element'remove-line row))
    (inc! version)))

(define (split-input-line! input-context row column)
  (with-slots (lines offset) input-context
    (let* ((line (get-input-line input-context row))
           (i (- column offset))
           (head (string-take line i))
           (tail (string-drop line i)))
      (insert-input-line! input-context row head)
      (set-input-line! input-context (+ row 1) tail))))

(define (concat-input-line! input-context row)
  (with-slots (lines) input-context
    (let* ((line1 (get-input-line input-context row))
           (line2 (get-input-line input-context (+ row 1)))
           (concat-line (string-append line1 line2)))
      (set-input-line! input-context row concat-line)
      (delete-input-line! input-context (+ row 1)))))

(define (end-of-line-column input-context row)
  (with-slots (offset) input-context
    (+ offset (string-length (get-input-line input-context row)))))

(define (get-prompt input-context row)
  (with-slots (prompter start-row) input-context
    (prompter (- row start-row))))

(define (get-input-content input-context)
  (with-slots (lines) input-context
    (apply string-append (intersperse "\n" (vector->list lines)))))

(define (draw-input-area input-context :optional (from-row #f) (to-row #f))
  (with-slots (text-element start-row end-row offset cursor-column cursor-row) input-context
    (let ((from-row (or from-row start-row))
          (to-row (or to-row end-row)))
      (do-ec
        (: row from-row (+ to-row 1))
        (begin
          (text-element'move-cursor 0 row)
          (text-element'erase-line 0)
          (text-element'print-text (get-prompt input-context row))
          (text-element'set-start-column)
          (text-element'move-cursor-horizontal-absolute offset)
          (text-element'print-text (get-input-line input-context row))))
      (text-element'move-cursor cursor-column cursor-row))))

(define (delete-mark-region input-context)
  (define (clip offset start-row end-row text-mark)
    (let-values (((from-col to-col) (min&max (text-mark-start-column text-mark) (text-mark-end-column text-mark)))
                 ((from-row to-row) (min&max (text-mark-start-row text-mark) (text-mark-end-row text-mark))))
      (cond
        ((< from-row to-row)
         (let ((from-row-pos (cond
                               ((< from-row start-row)
                                'above)
                               ((<= start-row from-row end-row)
                                'in)
                               (else
                                'below)))
               (to-row-pos (cond
                             ((< to-row start-row)
                              'above)
                             ((<= start-row to-row end-row)
                              'in)
                             (else
                              'below))))
           (match (list from-row-pos to-row-pos)
             ((or ('above 'above)
                  ('below 'below))
              (values #f #f #f #f))
             (('above 'in)
              (values offset start-row (max offset to-col) to-row))
             (('above 'below)
              (values offset start-row (end-of-line-column input-context end-row) end-row))
             (('in 'in)
              (values (max offset from-col) from-row (max offset to-col) to-row))
             (('in below)
              (values (max offset from-col) from-row (end-of-line-column input-context end-row) end-row)))))
        ((and (= from-row to-row)
              (<= offset to-col))
         (values (max offset from-col) from-row to-col to-row))
        (else
         (values #f #f #f #f)))))

  (with-slots (text-element start-row end-row cursor-column cursor-row offset) input-context
    (with-slots (text-mark) text-element
      (when text-mark
        (let-values (((from-col from-row to-col to-row) (clip offset start-row end-row text-mark)))
          (when (and from-col from-row to-col to-row)
            (cond
              ((= from-row to-row)
               (let* ((line (get-input-line input-context from-row))
                      (head (string-take line (- from-col offset)))
                      (tail (string-drop line (- to-col offset))))
                 (set-input-line! input-context from-row (string-append head tail))))
              (else
               (let* ((line (get-input-line input-context to-row))
                      (tail (string-drop line (max 0 (- to-col offset)))))
                 (set-input-line! input-context to-row tail))
               (do ((row (- to-row 1) (- row 1)))
                   ((<= row from-row) #f)
                 (delete-input-line! input-context row))
               (let* ((line (get-input-line input-context from-row))
                      (head (string-take line (max 0 (- from-col offset)))))
                 (set-input-line! input-context from-row head)
                 (concat-input-line! input-context from-row))))
            (set! cursor-column (max offset from-col))
            (set! cursor-row from-row)
            (clear-mark! input-context)
            (draw-input-area input-context from-row)))))))

(define (edit:insert-string input-context str)
  (with-slots (text-element) input-context
    (enqueue-input text-element 'text str)
    (process-queued-input input-context)))

(define (edit:newline-or-commit input-context)
  (delete-mark-region input-context)
  (with-slots (text-element input-continues offset cursor-column cursor-row) input-context
    (let1 text (get-input-content input-context)
      (cond
        ((or (and (procedure? input-continues)
                  (input-continues text))
             (and (not (procedure? input-continues))
                  input-continues))
         ;; newline
         (split-input-line! input-context cursor-row cursor-column)
         (set! cursor-row (+ cursor-row 1))
         (set! cursor-column offset)
         (draw-input-area input-context (- cursor-row 1)))
        (else
         ;; commit
         (finish-edit input-context 'commit))))))

(define (edit:cancel-edit input-context)
  (finish-edit input-context 'cancel))

(define (edit:previous-line input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (cond
      ((< start-row cursor-row)
       (if (or force-mark? (shift-pressed? input-context))
         (set-mark! input-context)
         (clear-mark! input-context))
       (set! cursor-row (- cursor-row 1))
       (text-element'move-cursor cursor-column cursor-row)
       (when (or force-mark? (shift-pressed? input-context))
         (set-mark! input-context)))
      (else
       #f))))

(define (edit:next-line input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (cond
      ((< cursor-row end-row)
       (if (or force-mark? (shift-pressed? input-context))
         (set-mark! input-context)
         (clear-mark! input-context))
       (set! cursor-row (+ cursor-row 1))
       (text-element'move-cursor cursor-column cursor-row)
       (when (or force-mark? (shift-pressed? input-context))
         (set-mark! input-context)))
      (else
       #f))))

(define (edit:previous-char input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (cond
      ((< offset cursor-column)
       (set! cursor-column (- cursor-column 1))
       (text-element'move-cursor cursor-column cursor-row))
      ((< start-row cursor-row)
       (set! cursor-row (- cursor-row 1))
       (set! cursor-column (end-of-line-column input-context cursor-row))
       (text-element'move-cursor cursor-column cursor-row))
      (else
       #f))
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:forward-char input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (cond
      ((< cursor-column (end-of-line-column input-context cursor-row))
       (set! cursor-column (+ cursor-column 1))
       (text-element'move-cursor cursor-column cursor-row))
      ((< cursor-row end-row)
       (set! cursor-row (+ cursor-row 1))
       (set! cursor-column offset)
       (text-element'move-cursor cursor-column cursor-row))
      (else
       #f))
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:beginning-of-line input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-column offset)
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:end-of-line input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-column (end-of-line-column input-context cursor-row))
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:beginning-of-edit-area input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-column offset)
    (set! cursor-row start-row)
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:end-of-edit-area input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-row end-row)
    (set! cursor-column (end-of-line-column input-context cursor-row))
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:page-up input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-row (max start-row (- cursor-row (~ text-element'page-size))))
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:page-down input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset force-mark?) input-context
    (if (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context)
      (clear-mark! input-context))
    (set! cursor-row (min end-row (+ cursor-row (~ text-element'page-size))))
    (text-element'move-cursor cursor-column cursor-row)
    (when (or force-mark? (shift-pressed? input-context))
      (set-mark! input-context))))

(define (edit:backward-delete-char input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset) input-context
    (with-slots (text-mark) text-element
      (cond
        (text-mark
         (delete-mark-region input-context))
        ((< offset cursor-column)
         (let* ((line (get-input-line input-context cursor-row))
                (i (- cursor-column offset))
                (head (string-take line (- i 1)))
                (tail (string-drop line i)))
           ;; Need to update cursor-column before set-input-line!.
           ;; Referencing cursor-column returns the length of the input line if the actual value exceeds the length.
           (dec! cursor-column)
           (set-input-line! input-context cursor-row (string-append head tail))
           (draw-input-area input-context cursor-row cursor-row)))
        ((< start-row cursor-row)
         (set! cursor-row (- cursor-row 1))
         (set! cursor-column (+ offset (string-length (get-input-line input-context cursor-row))))
         (concat-input-line! input-context cursor-row)
         (draw-input-area input-context cursor-row))
        (else
         #f)))))

(define (edit:delete-char input-context)
  (with-slots (text-element cursor-column cursor-row start-row end-row offset) input-context
    (with-slots (text-mark) text-element
      (cond
        (text-mark
         (delete-mark-region input-context))
        ((< cursor-column (end-of-line-column input-context cursor-row))
         (let* ((line (get-input-line input-context cursor-row))
                (i (- cursor-column offset))
                (head (string-take line i))
                (tail (string-drop line (+ i 1))))
           (set-input-line! input-context cursor-row (string-append head tail))
           (draw-input-area input-context cursor-row cursor-row)))
        ((< cursor-row end-row)
         (concat-input-line! input-context cursor-row)
         (draw-input-area input-context cursor-row))
        (else
         #f)))))

(define (edit:copy input-context)
  (with-slots (text-element) input-context
    (let1 text (text-element'extract-mark-region-text)
      (copy-text-to-clipboard text))))

(define (edit:cut input-context)
  (with-slots (text-element) input-context
    (let1 text (text-element'extract-mark-region-text)
      (copy-text-to-clipboard text)
      (delete-mark-region input-context))))

(define (edit:paste input-context)
  (let1 text (clipboard-text input-context)
    (edit:insert-string input-context text)))

(define (read-text/edit grv-text
                        :key
                        (prompt "")
                        (keymap #f)
                        (input-continues #f)
                        (initial-text #f)
                        ((:cursor-column initial-cursor-column) #f)
                        ((:cursor-row initial-cursor-row) #f)
                        (on-change #f)
                        (on-input #f))
  (let* ((row (grv-text'cursor-row))
         (cursor-visibility (grv-text'cursor-visible?))
         (keymap (or keymap (global-keymap)))
         (input-context (make <input-context>
                          :text-element grv-text
                          :row row
                          :input-continues input-continues
                          :keymap keymap
                          :prompter (cond
                                      ((string? prompt)
                                       (lambda _ prompt))
                                      ((list? prompt)
                                       (lambda (row)
                                         (list-ref prompt (min row (- (length prompt) 1)))))
                                      ((procedure? prompt)
                                       prompt)
                                      (else
                                       (errorf "<string>, <list> or <procedure> required, but got ~s" prompt))))))
    (define input-proc
      (let1 previous-version (~ input-context'version)
        (lambda (type str)
          (process-queued-input input-context)
          (when on-input
            (on-input input-context type str))
          (let1 ver (~ input-context'version)
            (when (and (not (equal? previous-version ver))
                       on-change)
              (on-change input-context))
            (set! previous-version ver)))))
    (define (clipboard-proc type)
      (when (eq? type 'cut)
        (delete-mark-region input-context)))
    (define (mouse-proc event-name col row)
      (with-slots (start-row end-row cursor-column cursor-row offset) input-context
        (when (and (<= start-row row end-row)
                   (<= offset col))
          (cond
            ((equal? event-name "S-Mouse-Click-0")
             (set! cursor-column col)
             (set! cursor-row row)
             (set-mark! input-context col row)
             (draw-input-area input-context row row))
            ((or (equal? event-name "Mouse-Click-0")
                 (equal? event-name "Mouse-Drag-0"))
             (set! cursor-column col)
             (set! cursor-row row)
             (draw-input-area input-context row row))
            (else
             #f)))))

    (with-slots (input-hook clipboard-hook mouse-hook) grv-text
      (unwind-protect
          (with-slots (offset cursor-column cursor-row edit-cont keymap this-key start-row end-row) input-context
            (add-hook! input-hook input-proc)
            (add-hook! clipboard-hook clipboard-proc)
            (add-hook! mouse-hook mouse-proc)

            (grv-text'save-key-event-availability)
            (switch-keymap input-context keymap)

            (grv-text'move-cursor-horizontal-absolute 0)
            (grv-text'erase-line 0)
            (grv-text'print-text (get-prompt input-context row))
            (grv-text'set-start-column)
            (set! offset (grv-text'cursor-column))
            (set! cursor-column offset)

            (when initial-text
              (clear-input-buffer! grv-text)
              (enqueue-input grv-text 'text initial-text)
              (process-queued-input input-context)
              ;; Reset the content version.
              (set! (~ input-context'version) 0))

            (when initial-cursor-row
              (set! cursor-row (min (+ start-row initial-cursor-row) end-row)))
            (when initial-cursor-column
              (set! cursor-column initial-cursor-column))

            (draw-input-area input-context)

            (grv-text'focus)
            (grv-text'show-cursor)
            (let-values (((text reason) (shift-callback callback
                                          (set! edit-cont callback))))
              (case reason
                ((commit)
                 text)
                (else
                 #f))))
        (grv-text'restore-key-event-availability)
        (delete-hook! input-hook input-proc)
        (delete-hook! clipboard-hook clipboard-proc)
        (delete-hook! mouse-hook mouse-proc)
        (unless cursor-visibility
          (grv-text'hide-cursor))))))
