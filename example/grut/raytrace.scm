;; This program is taken from ANSI Common Lisp by Paul Graham and is translated to Gauche.
(use gauche.parseopt)
(use gauche.record)
(use graviton)
(use graviton.grut)
(use util.match)

(define (sq x)
  (* x x))

(define (mag x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(define (unit-vector x y z)
  (let1 d (mag x y z)
    (values (/ x d) (/ y d) (/ z d))))

(define-record-type point
  #t #t
  (x x-of)
  (y y-of)
  (z z-of))

(define (distance p1 p2)
  (mag (- (x-of p1) (x-of p2))
       (- (y-of p1) (y-of p2))
       (- (z-of p1) (z-of p2))))

(define (minroot a b c)
  (if (zero? a)
    (/ (- c) b)
    (and-let* ((disc (- (sq b) (* 4 a c)))
               ((<= 0 disc))
               (discrt (sqrt disc)))
      (min (/ (+ (- b) discrt) (* 2 a))
           (/ (- (- b) discrt) (* 2 a))))))

(define-record-type surface #t #t
  color)

(define *world* (make-window-parameter '()))

(define-constant eye (make-point 0 0 200))

(define (tracer canvas)
  (let* ((ctx (canvas'get-context "2d"))
         (x-res (/ (~ canvas'width) 100))
         (y-res (/ (~ canvas'height) 100))
         (x-inc (/ x-res))
         (y-inc (/ y-res))
         (fill-color #f))
    (do ((y -50 (+ y y-inc)))
        ((< (- 50 y) y-inc))
      (do ((x -50 (+ x x-inc)))
          ((< (- 50 x) x-inc))
        (match-let1 (r g b) (color-at x y)
          (let1 color-name (format "rgb(~a,~a,~a)" r g b)
            (unless (equal? fill-color color-name)
              (set! (~ ctx'fill-style) (format "rgb(~a,~a,~a)" r g b))
              (set! fill-color color-name)))
          (ctx'fill-rect (round->exact (* (+ x 50) x-res)) (round->exact (* (+ y 50) y-res)) 2 2)
          (when-time-passed 0.1
            (asleep 0)))))))

(define (color-at x y)
  (receive (xr yr zr) (unit-vector (- x (x-of eye)) (- y (y-of eye)) (- 0 (z-of eye)))
    (sendray eye xr yr zr)
    (map (^v (round (* v 255)))
         (sendray eye xr yr zr))))

(define (sendray pt xr yr zr)
  (receive (s int) (first-hit pt xr yr zr)
    (if s
      (let1 l (lambert s int xr yr zr)
        (map (^v (* l v)) (surface-color s)))
      '(0 0 0))))

(define (first-hit pt xr yr zr)
  (let ((surface #f)
        (hit #f)
        (dist +inf.0))
    (dolist (s (*world*))
      (and-let* ((h (intersect s pt xr yr zr))
                 (d (distance h pt))
                 ((< d dist)))
        (set! surface s)
        (set! hit h)
        (set! dist d)))
    (values surface hit)))

(define (lambert s int xr yr zr)
  (receive (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(define-record-type (sphere surface) %make-sphere #t
  radius
  center)

(define (make-sphere x y z r c)
  (rlet1 s (%make-sphere c r (make-point x y z))
    (push! (*world*) s)))

(define-method intersect ((s sphere) pt xr yr zr)
  (and-let* ((c (sphere-center s))
             (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x-of pt) (x-of c)) xr)
                             (* (- (y-of pt) (y-of c)) yr)
                             (* (- (z-of pt) (z-of c)) zr)))
                     (+ (sq (- (x-of pt) (x-of c)))
                        (sq (- (y-of pt) (y-of c)))
                        (sq (- (z-of pt) (z-of c)))
                        (- (sq (sphere-radius s)))))))
    (make-point (+ (x-of pt) (* n xr)) (+ (y-of pt) (* n yr)) (+ (z-of pt) (* n zr)))))

(define-method normal ((s surface) pt)
  (let1 c (sphere-center s)
    (unit-vector (- (x-of c) (x-of pt)) (- (y-of c) (y-of pt)) (- (z-of c) (z-of pt)))))

(define (main args)
  (let-args (cdr args)
      ((pixels "p|pixels=i" 500)
       (force-player? "player" #f)
       (force-browser? "browser" #f)
       (force-server? "server" #f))
    (grv-config :mode (cond
                        (force-player? 'player)
                        (force-browser? 'browser)
                        (force-server? 'server)
                        (else #f)))
    (with-window (grut-canvas-window pixels pixels :background-color "black")
        (canvas)
      (on-jsevent window "keyup" (key)
        (when (equal? key "Escape")
          (close-window)))

      (make-sphere 0 -300 -1200 200 '(0.8 0 0))
      (make-sphere -80 -150 -1200 200 '(0 0.7 0))
      (make-sphere 70 -100 -1200 200 '(0 0 0.9))
      (do ((x -2 (+ x 1)))
          ((> x 2))
        (do ((z 2 (+ z 1)))
            ((> z 7))
          (make-sphere (* x 200) 300 (* z -400) 40 '(0.75 0.75 0.75))))
      (tracer canvas))))
