(use graviton)

(define (make-node freq)
  (let1 oscillator (audio-context'create-oscillator)
    (set! (~ oscillator'type) "square")
    (set! (~ oscillator'frequency'value) freq)
    (oscillator'connect (~ audio-context'destination))
    oscillator))

;; args := ((freq len) ...)
(define (play-wave args)
  (let1 nodes (map (lambda (arg)
                     (cons (make-node (car arg)) (cdr arg)))
                   args)
    (fold (lambda (node+len t)
            (let ((node (car node+len))
                  (len (cadr node+len)))
              (node'start t)
              (node'stop (+ t len))
              (+ t len)))
          (~ audio-context'current-time)
          nodes)))

(grv-window
  :path "/"

  (play-wave '((2000 0.1) (1000 0.1)))
  (worker-sleep! 1.0)
  (grv-exit))

(define (main args)
  (grv-start-player :show? #f))
