(use file.util)
(use gauche.parameter)
(use gauche.parseopt)
(use graviton)
(use graviton.grut)
(use srfi-1)
(use text.html-lite)
(use text.tree)

(bind-url-path "/editor.css" (build-path (sys-dirname (current-load-path)) "editor.css"))

(load-css "/editor.css")

(define-document-content
  (html:body
   (html:div :id "container"
             (html:div :id "win-0" :class "window"
                       (html:grv-text-edit :id "text-buffer" :class "text-buffer")
                       (html:grv-text :id "mode-line" :class "mode-line"))
             (html:grv-text :id "mini-buffer"))))

(define-global-jsobject text-buffer (document'get-element-by-id "text-buffer"))
(define-global-jsobject mode-line (document'get-element-by-id "mode-line"))
(define-global-jsobject mini-buffer (document'get-element-by-id "mini-buffer"))

;; (define *buffer-list* '())
(define *window-list* '())

;; (define *current-buffer* #f)
(define *selected-window* #f)

;; (define-class <buffer> ()
;;   ((name :init-keyword :name)
;;    (filename :init-value #f)
;;    (content :init-value "")))

(define-class <window> ()
  ((name :init-keyword :name)
   (filename :init-value #f)
   (div :init-keyword :div)
   (text-edit :init-keyword :text-edit)
   (mode-line :init-keyword :mode-line)
   (buffer :init-value #f)
   (last-update-timestamp :init-value 0)))

;; (define (current-buffer)
;;   *current-buffer*)

(define (selected-window)
  *selected-window*)

(define (select-window win)
  ((~ win'text-edit)'focus)
  ((~ win'text-edit)'show-cursor)
  (for-each (lambda (w)
              (unless (eq? w win)
                ((~ w'text-edit)'hide-cursor)))
            *window-list*)
  (set! *selected-window* win))

(define (get-window name)
  (any (^b (equal? (~ b'name) name)) *window-list*))

(define (generate-new-window-name starting-name)
  (define (make-unique-name cnt)
    (let1 name (string-append starting-name (if (= cnt 1)
                                     ""
                                     (format "<~d>" cnt)))
      (if (get-window starting-name)
        (make-unique-name (+ cnt 1))
        starting-name)))
  (make-unique-name 1))

(define (make-window div-id :key (name "*scratch*"))
  (let* ((div (document'get-element-by-id div-id))
         (text-edit ((div'query-selector-all ".text-buffer")'item 0))
         (mode-line ((div'query-selector-all ".mode-line")'item 0)))
    (rlet1 win (make <window> :div div :text-edit text-edit :mode-line mode-line :name (generate-new-window-name name))
      (push! *window-list* win))))

(define *div-id-counter* 1)

(define (init-first-window)
  (rlet1 win (make-window "win-0")
    (select-window win)))

(define (split-window :optional (win #f))
  (let* ((win (or win (selected-window)))
         (target-div (~ win'div))
         (new-div-id (begin0
                         (format "win-~d" *div-id-counter*)
                       (inc! *div-id-counter*))))
    (target-div'insert-adjacent-html "afterend" (tree->string
                                                  (html:div :id new-div-id :class "window"
                                                            (html:grv-text-edit :class "text-buffer")
                                                            (html:grv-text :class "mode-line"))))
    (make-window new-div-id)))

;; (define (buffer-name buffer)
;;   (~ buffer'name))

;; (define (rename-buffer newname :optional (unique #f))
;;   (cond
;;     ((and (get-buffer newname)
;;           (not unique))
;;      (message "buffer name '~a' is already used" newname)
;;      ;; (sit-for)
;;      ;; (minibuffer-clear)
;;      #f)
;;     (else
;;      (set! (~ (current-buffer) 'name) (generate-new-buffer-name newname)))))

;; (define (get-buffer buffer-or-name)
;;   (if (is-a? buffer-or-name <buffer>)
;;     buffer-or-name
;;     (any (^b (equal? (~ b'name) buffer-or-name)) *buffer-list*)))

;; (define (generate-new-buffer-name starting-name)
;;   (define (make-unique-name cnt)
;;     (let1 name (string-append starting-name (if (= cnt 1)
;;                                      ""
;;                                      (format "<~d>" cnt)))
;;       (if (get-buffer starting-name)
;;         (make-unique-name (+ cnt 1))
;;         starting-name)))
;;   (make-unique-name 1))

;; (define (generate-new-buffer name)
;;   (rlet1 buf (make <buffer> :name (generate-new-buffer-name name))
;;     (push! *buffer-list* buf)))

;; (define (get-buffer-create buffer-or-name)
;;   (if (is-a? buffer-or-name <buffer>)
;;     buffer-or-name
;;     (or (get-buffer buffer-or-name)
;;         (generate-new-buffer buffer-or-name))))

(define (get-file-window filename)
  (find (^w (equal? (~ w'filename) filename)) *window-list*))

(define (window-modified? win)
  (not (equal? (~ win'text-edit'last-update-timestamp) (~ win'last-update-timestamp))))

;; (define (set-window-buffer window buffer-name)
;;   (let1 buf (get-buffer-create buffer-name)
;;     (set! (~ window'buffer) buf)
;;     ((~ window'text-edit)'clear-text-content)
;;     ((~ window'text-edit)'insert-text (~ buf'content))))

;; (define (switch-to-buffer buffer-or-name)
;;   (let ((buf (get-buffer-create buffer-or-name))
;;         (win (selected-window)))
;;     (set! (~ win'buffer) buf)
;;     ((~ win'text-edit)'clear-text-content)
;;     ((~ win'text-edit)'insert-text (~ buf'content))))

(define (update-mode-line win)
  (let1 mode-line (~ win'mode-line)
    (mode-line'print-text (format "\x1b;[1G\x1b;[2K  ~a~a"
                                  (~ win'name)
                                  (if (and (~ win'filename)
                                           (window-modified? win))
                                    "*"
                                    " ")
                                  ))))

(define (message fmt :rest args)
  (minibuffer-clear)
  (mini-buffer'print-text (apply format fmt args)))

(define (sit-for :optional (sec #f))
  (jsevent-wait window "keydown" :jsproperties '() :timeout sec :use-capture? #t))

(define (minibuffer-clear)
  (mini-buffer'print-text "\x1b;[2J\x1b;[1;1H"))

(define (read-from-minibuffer prompt :optional (initial ""))
  (minibuffer-clear)
  (let loop ((initial initial)
             (pos (string-length initial)))
    (receive (str key) (mini-buffer'read-line :prompt prompt :content initial :position pos)
      (cond
        ((equal? key "Enter")
         (minibuffer-clear)
         (text-buffer'focus)
         str)
        (else
         (minibuffer-clear)
         (loop str (mini-buffer'cursor-column)))))))

(define (find-file :optional (filename #f))
  (let1 filename (or filename
                     (read-from-minibuffer "Find file: "))
    (guard (e (else (message (condition-message e "find-file error!!"))
                    #f))
      (or (get-file-window filename)
          (rlet1 win (selected-window)
            ;; TODO: Save the current content
            (set! (~ win'name) (generate-new-window-name (sys-basename filename)))
            (set! (~ win'filename) filename)
            (cond
              ((file-exists? filename)
               (set! (~ win'text-edit'text-content) (call-with-input-file filename port->string))
               (set! (~ win'last-update-timestamp) (~ win'text-edit'last-update-timestamp)))
              (else
               (set! (~ win'text-edit'text-content) "")
               (message "New file")
               (sit-for)
               (minibuffer-clear))))))))

(define (save-window)
  (let* ((win (selected-window))
         (filename (or (~ win'filename)
                       (rlet1 filename (read-from-minibuffer "File to save in: ")
                         (set! (~ win'filename) filename)
                         (set! (~ win'name) (generate-new-window-name (sys-basename filename))))))
         (content (~ win'text-edit'text-content)))
    (call-with-output-file filename (cut display content <>))
    (set! (~ win'last-update-timestamp) (~ win'text-edit'last-update-timestamp))))

(define (write-file)
  (let* ((win (selected-window))
         (filename (rlet1 filename (read-from-minibuffer "Write file: ")
                     (set! (~ win'filename) filename)
                     (set! (~ win'name) (generate-new-window-name (sys-basename filename)))))
         (content (~ win'text-edit'text-content)))
    (call-with-output-file filename (cut display content <>))
    (set! (~ win'last-update-timestamp) (~ win'text-edit'last-update-timestamp))))

(define (main args)
  (let-args (cdr args)
      ((browser? "browser" #f))

    (if browser?
      (grv-browser)
      (grv-player :resizable? #t)))

  (grv-log-config :log-level 1)

  (grv-begin
    (select-window (init-first-window))

    (let1 c-x-map (make-key-map)
      (c-x-map'bind "C-c" (^()
                            (set! (~ document'body'inner-html) "")
                            (grv-exit)))
      (c-x-map'bind "C-f" find-file)
      (c-x-map'bind "C-s" save-window)
      (c-x-map'bind "C-w" write-file)
      (c-x-map'bind-fallback minibuffer-clear)
      (grv-text-edit-key-map'bind "C-x" (vector c-x-map (lambda ()
                                                          (message "C-x")))))

    (on-idle ()
      (for-each update-mode-line *window-list*))
    (worker-thread-idle-timeout 0.1)
    )
  )
