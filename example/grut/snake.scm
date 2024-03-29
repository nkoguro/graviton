(use gauche.parseopt)
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

(define (field-all-update! field)
  (let ((obj-tbl (~ field'object-table))
        (upd-tbl (~ field'update-table)))
    (hash-table-clear! upd-tbl)
    (hash-table-for-each obj-tbl (lambda (key obj) (hash-table-put! upd-tbl key obj)))
    (hash-table-clear! obj-tbl)))

(define (field-updates-for-each field proc)
  (let ((obj-tbl (~ field'object-table))
        (upd-tbl (~ field'update-table)))
    (hash-table-for-each upd-tbl (lambda (key obj)
                                   (unless (eq? obj (hash-table-get obj-tbl key #f))
                                     (proc (car key) (cdr key) obj)
                                     (hash-table-put! obj-tbl key obj))))
    (hash-table-clear! upd-tbl)))

(define (init-snake state)
  (set! (~ state'snake)
        (let ((mx (round->exact (/. *field-width* 2)))
              (my (round->exact (/. *field-height* 2))))
          (list (cons mx my) (cons (+ mx 1) my))))
  (set! (~ state'dx) -1)
  (set! (~ state'dy) 0))

(define (draw-snake state)
  (let ((field (~ state'field))
        (snake (~ state'snake)))
    (match-let1 (x . y) (car snake)
      (field-set! field x y 'head))
    (for-each (match-lambda
                ((x . y)
                 (field-set! field x y 'body)))
              (cdr snake))))

(define (erase-snake state)
  (let ((field (~ state'field))
        (snake (~ state'snake)))
    (for-each (match-lambda
                ((x . y)
                 (field-set! field x y #f)))
              snake)))

(define (move-snake state)
  (let1 hit? #f
    (set! (~ state'snake)
          (let* ((field (~ state'field))
                 (snake (~ state'snake))
                 (snake-body snake)
                 (snake-head (car snake))
                 (x (+ (car snake-head) (~ state'dx)))
                 (y (+ (cdr snake-head) (~ state'dy)))
                 (new-head (cons x y)))
            (case (field-get field x y)
              ((wall body)
               (play-beep 100 0.1)
               (wait-all-tracks)
               (set! hit? #t)
               (cons new-head (drop-right snake-body 1)))
              ((food)
               (inc! (~ state'score) 10)
               (play-beep 2000 0.05)
               (put-food state)
               (cons new-head snake-body))
              (else
               (cons new-head (drop-right snake-body 1))))))
    (draw-snake state)
    hit?))

(define (put-food state)
  (let1 field (~ state'field)
    (let ((x (random-integer *field-width*))
          (y (random-integer *field-height*)))
      (case (field-get field x y)
        ((#f)
         (field-set! field x y 'food))
        (else
         (put-food state))))))

(define (init-field state)
  (let1 field (~ state'field)
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
        (field-set! field x (- *field-height* 1 i) 'wall)))))

(define (render state)
  (let ((field (~ state'field))
        (text (~ state'text)))
    (field-updates-for-each field
      (lambda (x y obj)
        (move-cursor-to text y x)
        (case obj
          ((wall)
           (set-character-attribute text '(cyan blue bright))
           (display "#" text))
          ((head)
           (set-character-attribute text '(green black bright))
           (display "@" text))
          ((body)
           (set-character-attribute text '(green black bright))
           (display "*" text))
          ((food)
           (set-character-attribute text '(white black bright))
           (display "O" text))
          ((#f)
           (set-character-attribute text '(white black bright))
           (display " " text)))))
    (move-cursor-to text (- *text-height* 1) 0)
    (set-character-attribute text '(white black bright))
    (format text "SCORE: ~4,'0d" (~ state'score))))

(define (show-string-in-center text str row)
  (receive (w h) (query-screen-size text)
    (move-cursor-to text row (floor->exact (/ (- w (string-length str)) 2)))
    (display str text)))

(define (show-title text)
  (clear-screen text)
  (set-character-attribute text '(white black bright))
  (show-string-in-center text "S N A K E" 5)
  (show-string-in-center text "    K    " 9)
  (show-string-in-center text "    \u2191    " 10)
  (show-string-in-center text "H \u2190 @ \u2192 L" 11)
  (show-string-in-center text "    \u2193    " 12)
  (show-string-in-center text "    J    " 13)
  (show-string-in-center text "HIT ANY KEY" 16))

(define (show-gameover text)
  (set-character-attribute text '(white black bright))
  (show-string-in-center text "GAME OVER" 5)
  (show-string-in-center text " HIT ANY KEY " 10))

(define-class <game-state> ()
  ((field :init-form (make <field>))
   (snake :init-value '())
   (dx :init-value -1)
   (dy :init-value 0)
   (score :init-value 0)
   (scene :init-value 'start)
   (text :init-keyword :text)))

(define (init-game state)
  (init-field state)
  (init-snake state)
  (set! (~ state'score) 0)
  (draw-snake state)
  (put-food state)
  (clear-screen (~ state'text))

  (render state))

(define (scene-change state next)
  (let1 text (~ state'text)
    (while (chready? text)
      (getch text))

    (case next
      ((title)
       (show-title text))
      ((game)
       (init-game state)
       (show-string-in-center text "START !!" 8)
       (play-mml :main '(t180 v0.4 :adsr (0 0 1 0.01) dbg r8 > d16 r16 g r8 d16 r16 g16 r16 d16 r16 g16 r16 b16 r16 > d r4
                              c r8 < a16 r16 > c r8 < a16 r16 > c16 r16 < a16 r16 f+16 r16 a16 r16 d r8))
       (wait-all-tracks)
       (field-all-update! (~ state'field))
       (clear-screen text)
       (render state))
      ((gameover)
       (show-gameover text)
       (play-mml :main '(t120 v0.7 :adsr (0 0 1 0.01) < b- b-8. b-16 b- > d-8. c16 c8. < b-16 b-8. b-16 b-2))
       (wait-all-tracks))
      (else
       #f))

    (set! (~ state'scene) next)))

(define (scene-title state)
  (let1 text (~ state'text)
    (match (and (chready? text)
                (getch text))
      (#f
       'title)
      (#\x1b
       (close-window))
      (_
       'game))))

(define (scene-game state)
  (let ((field (~ state'field))
        (text (~ state'text)))
    (erase-snake state)

    (match (let loop ((ch #f))
             (if (chready? text)
               (loop (getch text))
               ch))
      ((or #\h #\a 'KEY_LEFT)
       (when (= (~ state'dx) 0)
         (set! (~ state'dx) -1)
         (set! (~ state'dy) 0)))
      ((or #\j #\s 'KEY_DOWN)
       (when (= (~ state'dy) 0)
         (set! (~ state'dx) 0)
         (set! (~ state'dy) 1)))
      ((or #\k #\w 'KEY_UP)
       (when (= (~ state'dy) 0)
         (set! (~ state'dx) 0)
         (set! (~ state'dy) -1)))
      ((or #\l #\d 'KEY_RIGHT)
       (when (= (~ state'dx) 0)
         (set! (~ state'dx) 1)
         (set! (~ state'dy) 0)))
      (#\x1b
       (close-window))
      (_
       #f))
    (let1 hit? (move-snake state)
      (render state)
      (if hit?
        'gameover
        'game))))

(define (scene-gameover state)
  (let1 text (~ state'text)
    (match (and (chready? text)
                (getch text))
      (#f
       'gameover)
      (#\x1b
       (close-window))
      (_
       'game))))

(define (scene-dispatch state)
  (let1 next-scene (case (~ state'scene)
                     ((start)
                      'title)
                     ((title)
                      (scene-title state))
                     ((game)
                      (scene-game state))
                     ((gameover)
                      (scene-gameover state)))
    (unless (eq? (~ state'scene) next-scene)
      (scene-change state next-scene))))

(define *interval-sec* 0.2)

(define (main args)
  (let-args (cdr args)
      ((force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))

    (with-window (grut-text-window :column *text-width* :row *text-height* :fit 'fill :window-width 600 :window-height 600)
        (text-console)
      (let1 state (make <game-state> :text text-console)
        (call-with-console text-console
          (lambda (con)
            (while #t
              (scene-dispatch state)
              (asleep *interval-sec*))))))))
