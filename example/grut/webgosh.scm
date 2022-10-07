(use file.util)
(use gauche.hook)
(use gauche.parameter)
(use gauche.parseopt)
(use gauche.sequence)
(use graviton)
(use graviton.grut)
(use srfi-1)
(use srfi-13)
(use text.html-lite)
(use util.match)

(define css-path (build-path (sys-dirname (current-load-path)) "webgosh.css"))

(define (input-continues? input-context)
  (let1 str (input-context-text-content input-context)
    (rlet1 continue? (cond
                       ((#/^\s*,\S+/ str)
                        #f)
                       (else
                        (guard (e ((and (<read-error> e)
                                        (string-contains (condition-message e "") "EOF inside a list"))
                                   #t)
                                  (else
                                   #f))
                          (read-from-string str)
                          #f)))
      (unless continue?
        (end-completion input-context)))))

(define (make-prompt)
  (let* ((m ((with-module gauche.internal vm-current-module)))
         (prompt (cond
                   ((eq? m (find-module 'user))
                    "gosh$ ")
                   (else
                    (format "gosh[~a]$ " (module-name m))))))
    (list prompt (string-append (make-string (- (string-length prompt) 1) #\.) " "))))

;;;


(define (extract-common-prefix strs)
  (let loop ((prefix (car strs))
             (rest (cdr strs)))
    (cond
      ((null? rest)
       prefix)
      (else
       (let* ((str (car rest))
              (i (string-prefix-length prefix str)))
         (loop (substring prefix 0 i) (cdr rest)))))))

(define-class <completion-state> ()
  ((row :init-keyword :row)
   (start-index :init-keyword :start-index)
   (all-words :init-value '())
   (candidates :init-value '())
   (input-word :init-value "")
   (select-row :init-value 0)))

(define (show-completion-window input-context column)
  (let ((console (~ input-context'data'console-element))
        (completion (~ input-context'data'completion-element)))
    (receive (x y w h) (compute-character-position&size console column (~ input-context'cursor-row))
      (let-jsproperties (completion'get-bounding-client-rect)
          ((completion-w width) (completion-h height))
        (let-jsproperties (console'get-bounding-client-rect)
            ((console-x x) (console-y y) (console-w width) (console-h height))
          (set! (~ completion'style'left) (min x (- (+ console-x console-w) completion-w)))
          (set! (~ completion'style'top) (let1 top (+ y h)
                                             (if (< (+ top h)  (+ console-y console-h))
                                               top
                                               (- y completion-h))))
          (set! (~ completion'style'visibility) "visible"))))))

(define (hide-completion-window input-context)
  (let ((completion (~ input-context'data'completion-element)))
    (set! (~ completion'style'visibility) "hidden")))

(define *word-char-set* #[!$%&*+\-./:<=>?@\^~0-9A-Za-z])

(define (start-completion input-context)
  (let* ((cursor-column (~ input-context'cursor-column))
         (cursor-row (~ input-context'cursor-row))
         (offset (~ input-context'offset))
         (line (input-context-text-line input-context))
         (start-index (+ (or (string-skip-right line *word-char-set* 0 (- cursor-column offset)) -1) 1))
         (start-column (+ start-index offset))
         (state (make <completion-state>
                  :row cursor-row
                  :start-index start-index)))
    (set! (~ input-context'data'completion-state) state)
    (let ((m ((with-module gauche.internal vm-current-module)))
          (tbl (make-hash-table)))
      (for-each (lambda (sym)
                  (hash-table-put! tbl sym #t))
                (hash-table-keys (module-table m)))
      (for-each (lambda (m)
                  (for-each (lambda (sym)
                              (hash-table-put! tbl sym #t))
                            (hash-table-keys (module-table m))))
                (module-precedence-list m))
      (for-each (lambda (m)
                  (for-each (lambda (sym)
                              (hash-table-put! tbl sym #t))
                            (module-exports m)))
                (module-imports m))
      (set! (~ state'all-words) (sort (remove (^s (string-contains s " ")) (map symbol->string (hash-table-keys tbl)))))
      (add-hook! (~ input-context'data'hook) update-completion-by-input)
      (switch-keymap input-context (~ input-context'data'completion-keymap))
      (update-completion input-context #t))))

(define *right-padding* 5)
(define *max-win-rows* 10)

(define (render-completion input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((completion (~ input-context'data'completion-element))
           (candidates (~ state'candidates))
           (win-cols (+ (apply max (map string-length candidates)) *right-padding*))
           (win-rows (min *max-win-rows* (length candidates))))
      (clear-screen completion)
      (set! (~ completion'column) win-cols)
      (set! (~ completion'row) win-rows)
      (for-each-with-index (lambda (i str)
                             (set-line-style! completion
                                              i
                                              "background-color"
                                              (if (= i (~ state'select-row))
                                                "blue"
                                                #f))
                             (display str completion)
                             (newline completion)
                             (reset-character-attribute completion))
                           candidates)
      (scroll-to completion (~ state'select-row) #f)
      (show-completion-window input-context (+ (~ state'start-index) (~ input-context'offset))))))

(define (insert-common-prefix input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((line (input-context-text-line input-context))
           (start (~ state'start-index))
           (end (- (~ input-context'cursor-column) (~ input-context'offset)))
           (str (substring line start end))
           (candidates (~ state'candidates))
           (prefix (extract-common-prefix (~ state'candidates)))
           (tail (if (< (string-length str) (string-length prefix))
                   (string-copy prefix (string-length str))
                   "")))
      (set! (~ state'input-word) prefix)
      (edit:insert-string input-context tail)
      (when (= (length candidates) 1)
        (end-completion input-context)))))

(define (update-completion input-context auto-insert?)
  (and-let1 state (~ input-context'data'completion-state)
    (let* ((row (~ state'row))
           (start (~ state'start-index))
           (end (- (~ input-context'cursor-column) (~ input-context'offset)))
           (line (input-context-text-line input-context row))
           (str (if (< start end)
                  (substring line start end)
                  ""))
           (completion (~ input-context'data'completion-element))
           (all-words (~ state'all-words))
           (candidates (filter (cut string-prefix? str <>) all-words)))
      (set! (~ state'candidates) candidates)
      (set! (~ state'input-word) str)
      (cond
        ((or (null? candidates) (= (string-length str) 0))
         (end-completion input-context))
        (auto-insert?
         (insert-common-prefix input-context)
         (if (= (length candidates) 1)
           (end-completion input-context)
           (render-completion input-context)))
        (else
         (render-completion input-context))))))

(define (update-completion-by-input input-context type str)
  (or (and-let1 state (~ input-context'data'completion-state)
        (let* ((row (~ state'row))
               (start-column (+ (~ state'start-index) (~ input-context'offset)))
               (cursor-column (~ input-context'cursor-column))
               (cursor-row (~ input-context'cursor-row)))
          (cond
            ((and (= row cursor-row)
                  (< start-column cursor-column))
             (update-completion input-context #f))
            (else
             (end-completion input-context))))
        #t)
      (end-completion input-context)))

(define (end-completion input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (hide-completion-window input-context)
    (switch-keymap input-context (global-keymap))
    (set! (~ input-context'data'completion-state) #f)
    (remove-hook! (~ input-context'data'hook) update-completion-by-input)))

(define (indent-or-completion input-context)
  (let ((state (~ input-context'data'completion-state))
        (str (string-copy (input-context-text-line input-context) 0 (- (~ input-context'cursor-column)
                                                                       (~ input-context'offset)))))
    (cond
      ((and state (null? (~ state'candidates)))
       (end-completion input-context))
      (state
       (insert-common-prefix input-context))
      ((= (string-length (string-trim-both str)) 0)
       ;; TODO: Add indent
       (edit:insert-string input-context "  "))
      (else
       (start-completion input-context)))))

(define (next-candidate input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (inc! (~ state'select-row))
    (when (<= (length (~ state'candidates)) (~ state'select-row))
      (set! (~ state'select-row) 0))))

(define (prev-candidate input-context)
  (and-let1 state (~ input-context'data'completion-state)
    (dec! (~ state'select-row))
    (when (< (~ state'select-row) 0)
      (set! (~ state'select-row) (- (length (~ state'candidates)) 1)))))

(define (commit-completion input-context)
  (let* ((state (~ input-context'data'completion-state))
         (input-word (~ state'input-word))
         (select-row (~ state'select-row))
         (selected-word (list-ref (~ state'candidates) select-row))
         (tail (string-copy selected-word (string-length input-word))))
    (edit:insert-string input-context tail)
    (end-completion input-context)))

;;;

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f)
       (font-size "font-size=s" #f)
       (show-help? "h|help" #f))
    (when show-help?
      (print #"Usage: ~(car args) [Options]")
      (print "Options:")
      (print "  --player          Use graviton-player.")
      (print "  --browser         Use Web browser.")
      (print "  --server          Run as server mode.")
      (print "  --font-size=size  Change the font size.")
      (exit 0))
    (grv-log-config :log-level 1)

    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))

    (with-window
        (grv-window
          :css (file->url css-path)
          :title "gosh on Web"
          :body
          (html:body
           :class "grut-monospace-font"
           (html:div
            :id "container"
            :style (alist->style `(("font-size" . ,font-size)))
            (html:grut-text :id "console"))
           (html:grut-text :id "completion" :style (alist->style `(("font-size" . ,font-size) ("visibility" . "hidden"))))))
        (console status completion)

      (let* ((hook (make-hook 3))
             (completion-keymap (make-keymap (global-keymap))))
        (parameterize ((current-output-port console)
                       (current-error-port console)
                       (current-trace-port console))
          (bind-key (global-keymap) "Tab" indent-or-completion)

          (bind-key completion-keymap "ArrowUp" prev-candidate)
          (bind-key completion-keymap "ArrowDown" next-candidate)
          (bind-key completion-keymap "Escape" end-completion)
          (bind-key completion-keymap "Enter" commit-completion)

          (format #t "Gauche version ~a~%" (gauche-version))
          (format #t "You can use Tab Completion.~%~%")

          (grv-repl
            (lambda ()
              (let loop ()
                (let1 str (read-text/edit console
                                          :prompt (make-prompt)
                                          :input-continues input-continues?
                                          :data-alist `((console-element . ,console)
                                                        (completion-element . ,completion)
                                                        (completion-state . #f)
                                                        (completion-keymap . ,completion-keymap)
                                                        (hook . ,hook))
                                          :on-input (lambda (ctx type str)
                                                      (run-hook hook ctx type str)))
                  (cond
                    ((or (not str)
                         (= (string-length (string-trim-both str)) 0))
                     (loop))
                    (else
                     (read-from-string str))))))
            #f
            #f
            (lambda ()
              #f)))))))
