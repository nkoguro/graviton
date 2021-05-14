
(use graviton)
(use graviton.grut)
(use srfi-1)
(use srfi-27)
(use text.html-lite)
(use util.match)

(define *text-width* 20)
(define *text-height* 20)
(define *field-width* *text-width*)
(define *field-height* (- *text-height* 1))

(define-global-jsobject text (document'get-element-by-id "text"))

(grv-window
  :path "/"
  :body
  (html:body
   :style "background-color: black; color: white"
   (html:grv-text :id "text" :data-width *text-width* :data-height *text-height* :class "grut-monospace-font grut-fill"))

  (let ((field (make <field>))
        (snake '())
        (dx -1)
        (dy 0)
        (score 0)
        (last-key #f)
        (sec 0)
        (scene 'title))
    (define (init-game)
      (text'erase-display 2)
      (init-field field)
      (set! snake (make-snake))
      (set! dx -1)
      (set! dy 0)
      (set! score 0)
      (draw-snake field snake)
      (put-food field)
      (render-field field))

    (on-jsevent window "keydown" (key)
      (cond
        ((equal? key "Escape")
         (grv-exit))
        (else
         (set! last-key key))))

    (on-repaint (sec-per-frame)
      (inc! sec sec-per-frame)
      (when (<= *interval-sec* sec)
        (case scene
          ((title)
           (render-title)
           (when last-key
             (init-game)
             (set! scene 'game)))
          ((game)
           (erase-snake field snake)

           (match last-key
             ((or "h" "a" "ArrowLeft")
              (when (= dx 0)
                (set! dx -1)
                (set! dy 0)))
             ((or "j" "s" "ArrowDown")
              (when (= dy 0)
                (set! dx 0)
                (set! dy 1)))
             ((or "k" "w" "ArrowUp")
              (when (= dy 0)
                (set! dx 0)
                (set! dy -1)))
             ((or "l" "d" "ArrowRight")
              (when (= dx 0)
                (set! dx 1)
                (set! dy 0)))
             (_
              #f))

           (set! snake (let* ((snake-body snake)
                              (snake-head (car snake))
                              (x (+ (car snake-head) dx))
                              (y (+ (cdr snake-head) dy))
                              (new-head (cons x y)))
                         (case (field-get field x y)
                           ((wall body)
                            (beep 100 0.1)
                            (set! scene 'gameover)
                            (cons new-head (drop-right snake-body 1)))
                           ((food)
                            (inc! score 10)
                            (beep 2000 0.05)
                            (put-food field)
                            (cons new-head snake-body))
                           (else
                            (cons new-head (drop-right snake-body 1))))))
           (draw-snake field snake)

           (render-field field)
           (text'move-cursor 0 (- *text-height* 1))
           (text'set-color "white" "black")
           (text'print-text (format "SCORE: ~4,'0d" score)))
          ((gameover)
           (render-gameover)
           (when last-key
             (init-game)
             (set! scene 'game))))

        (set! last-key #f)
        (set! sec 0)))))

(define-class <field> ()
  ((object-table :init-form (make-hash-table 'equal?))
   (update-table :init-form (make-hash-table 'equal?))))

(define (field-key x y)
  (cons x y))

(define (field-key->x+y key)
  (values (car key) (cdr key)))

(define (field-get field x y)
  (let1 key (field-key x y)
    (or (hash-table-get (~ field'update-table) key #f)
        (hash-table-get (~ field'object-table) key #f))))

(define (field-set! field x y obj)
  (hash-table-put! (~ field'update-table) (field-key x y) obj))

(define (field-clear! field)
  (hash-table-clear! (~ field'object-table))
  (hash-table-clear! (~ field'update-table)))

(define (field-updates-for-each field proc)
  (let ((obj-tbl (~ field'object-table))
        (upd-tbl (~ field'update-table)))
    (hash-table-for-each upd-tbl (lambda (key obj)
                                   (unless (eq? obj (hash-table-get obj-tbl key #f))
                                     (proc (car key) (cdr key) obj)
                                     (hash-table-put! obj-tbl key obj))))
    (hash-table-clear! upd-tbl)))

(define (make-snake)
  (let ((mx (round->exact (/. *field-width* 2)))
        (my (round->exact (/. *field-height* 2))))
    (list (cons mx my) (cons (+ mx 1) my))))

(define (draw-snake field snake)
  (match-let1 (x . y) (car snake)
    (field-set! field x y 'head))
  (for-each (match-lambda
              ((x . y)
               (field-set! field x y 'body)))
            (cdr snake)))

(define (erase-snake field snake)
  (for-each (match-lambda
              ((x . y)
               (field-set! field x y #f)))
            snake))

(define (put-food field)
  (let ((x (random-integer *field-width*))
        (y (random-integer *field-height*)))
    (case (field-get field x y)
      ((#f)
       (field-set! field x y 'food))
      (else
       (put-food field)))))

(define (init-field field)
  (field-clear! field)
  (dotimes (x *field-width*)
    (field-set! field x 0 'wall)
    (field-set! field x (- *field-height* 1) 'wall))
  (dotimes (y *field-height*)
    (field-set! field 0 y 'wall)
    (field-set! field (- *field-width* 1) y 'wall))
  (let ((h (round->exact (/. *field-height* 3)))
        (x (round->exact (/. *field-width* 2))))
    (dotimes (i h)
      (field-set! field x i 'wall)
      (field-set! field x (- *field-height* 1 i) 'wall))))

(define (render-field field)
  (field-updates-for-each field
    (lambda (x y obj)
      (text'move-cursor x y)
      (case obj
        ((wall)
         (text'set-color "cyan" "blue")
         (text'print-text "#"))
        ((head)
         (text'set-color "lime" "black")
         (text'print-text "@"))
        ((body)
         (text'set-color "lime" "black")
         (text'print-text "*"))
        ((food)
         (text'set-color "white" "black")
         (text'print-text "O"))
        ((#f)
         (text'set-color "white" "black")
         (text'print-text " "))))))

(define *interval-sec* 0.2)

(define (render-string-in-center str row)
  (text'move-cursor (floor->exact (/ (- *text-width* (string-length str)) 2)) row)
  (text'print-text str))

(define (render-title)
  (text'erase-display 2)
  (text'move-cursor 0 0)
  (text'set-color "white" "black")
  (render-string-in-center "S N A K E" 5)

  (render-string-in-center "HIT ANY KEY" 10))

(define (render-gameover)
  (text'set-color "white" "black")
  (render-string-in-center "GAME OVER" 5)

  (render-string-in-center " HIT ANY KEY " 10))

(define (main args)
  ;; (grv-browser)
  (grv-start-player :window-size '(800 800)))