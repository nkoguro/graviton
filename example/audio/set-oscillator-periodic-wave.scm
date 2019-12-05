(use graviton2)

(define (main args)
  (grv-begin
    (let ((oscillator (make-oscillator-node))
          (destination (make-audio-context-destination)))
      (set-oscillator-periodic-wave! oscillator #(0 1) #(0 0) :disable-nomalization #t)
      (connect-node! oscillator destination)
      (set-audio-base-time!)
      (start-audio-node! oscillator)
      (stop-audio-node! oscillator 2)

      (asleep 2.5)
      (app-close)))
  0)
