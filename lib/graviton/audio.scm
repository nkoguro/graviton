;;;
;;; audio.scm - Audio
;;;
;;;   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.audio
  (use gauche.collection)
  (use gauche.record)
  (use gauche.uvector)
  (use graviton.async)
  (use graviton.common)
  (use math.const)
  (use parser.peg)
  (use srfi-42)
  (use util.match)

  (export-all))

(select-module graviton.audio)
(dynamic-load "graviton-audio")

;;;
;;; MML
;;;

(define (pitch n)
  (* 440 (expt 2 (/. (- n 69) 12))))

(define-record-type (<envelope> (pseudo-rtd <list>))
  make-envelope envelope?
  (attack-time envelope-attack-time)
  (decay-time envelope-decay-time)
  (sustain-level envelope-sustain-level)
  (release-time envelope-release-time))

(define (generate-make-simple-tone type)
  (lambda (freq amp-factor vols sec envelope)
    (make-soundlet type
                   (make-f64vector 1 freq)
                   (make-f64vector 1 amp-factor)
                   (list-ref vols 0)
                   (list-ref vols 1)
                   sec
                   (envelope-attack-time envelope)
                   (envelope-decay-time envelope)
                   (envelope-sustain-level envelope)
                   (envelope-release-time envelope))))

(define make-default-tone (generate-make-simple-tone 'sine))

(define (make-silent sec)
  (make-soundlet 'silent #f64() #f64() 0 0 sec 0 0 0 0))

(define default-envelope (make-envelope 0 0 1.0 0))

(define (merge-soundlet seq)
  (fold (lambda (former latter)
          (when latter
            (append-soundlet! former latter))
          former)
        #f
        seq))

(define (stereo-volumes panning)
  (list (cos (* pi/4 (+ panning 1)))
        (sin (* pi/4 (+ panning 1)))))

(define (compile-mml context seq mml cont)
  (match mml
    (()
     (cont context seq))
    ((('wave type freq vel sec) rest ...)
     (let ((make-tone (generate-make-simple-tone type))
           (vols (stereo-volumes (assoc-ref context 'panning 0)))
           (envelope (assoc-ref context 'envelope default-envelope)))
       (compile-mml
         context
         (cons (make-tone freq vel vols sec envelope) seq)
         rest
         cont)))
    ((('note n v sec) rest ...)
     (let ((make-tone (assoc-ref context 'make-tone make-default-tone))
           (vols (stereo-volumes (assoc-ref context 'panning 0)))
           (envelope (assoc-ref context 'envelope default-envelope)))
       (compile-mml
         context
         (cons (make-tone (pitch n) v vols sec envelope) seq)
         rest
         cont)))
    ((('rest sec) rest ...)
     (compile-mml
       context
       (cons (make-silent sec) seq)
       rest
       cont))
    ((('compose mmls ...) rest ...)
     (compile-mml
       context
       (cons (compose-soundlets (filter values (map (lambda (mml)
                                                      (compile-mml context '() mml (lambda (_ seq)
                                                                                     (merge-soundlet seq))))
                                                    mmls)))
             seq)
       rest
       cont))
    ((('begin mml ...) rest ...)
     (let1 current-context (map (lambda (pair)
                                  (list-copy pair))
                                context)
       (compile-mml context seq mml (lambda (context seq)
                                      (compile-mml current-context seq rest cont)))))
    ((('pan v) rest ...)
     (compile-mml
       (assoc-set! context 'panning v)
       seq
       rest
       cont))
    ((('tone (? symbol? type)) rest ...)
     (compile-mml
       (assoc-set! context 'make-tone (generate-make-simple-tone type))
       seq
       rest
       cont))
    ((('tone (freq-coff amp) ...) rest ...)
     (compile-mml
       (assoc-set! context 'make-tone (lambda (freq amp-factor vols sec envelope)
                                        (make-soundlet type
                                                       (list->f64vector (map (cut (* freq <>)) freq-coff))
                                                       (list->f64vector (map (^x (* amp amp-factor)) amp))
                                                       (list-ref vols 0)
                                                       (list-ref vols 1)
                                                       sec
                                                       (envelope-attack-time envelope)
                                                       (envelope-decay-time envelope)
                                                       (envelope-sustain-level envelope)
                                                       (envelope-release-time envelope))))
       seq
       rest
       cont))
    ((('envelope attack-time decay-time sustain-level release-time) rest ...)
     (compile-mml
       (assoc-set! context 'envelope (make-envelope attack-time decay-time sustain-level release-time))
       seq
       rest
       cont))
    ((('length n) rest ...)
     (compile-mml
       (assoc-set! context 'length (/ 1 n))
       seq
       rest
       cont))
    ((('velocity v) rest ...)
     (compile-mml
       (assoc-set! context 'amp-factor v)
       seq
       rest
       cont))
    ((('tempo n) rest ...)
     (compile-mml
       (assoc-set! context 'tempo-factor (* (/ 60 n) 4))
       seq
       rest
       cont))
    ((('tempo n m) rest ...)
     (compile-mml
       (assoc-set! context 'tempo-factor (* (/ 60 n) m))
       seq
       rest
       cont))
    ((('octave n) rest ...)
     (compile-mml
       (assoc-set! context 'octave n)
       seq
       rest
       cont))
    (((? symbol? note-spec) rest ...)
     (compile-mml
       context
       (cons (note-spec->soundlet context (symbol->string note-spec)) seq)
       rest
       cont))
    (((? string? note-spec) rest ...)
     (compile-mml
       context
       (cons (note-spec->soundlet context note-spec) seq)
       rest
       cont))
    (else
     (errorf "Invalied mml: ~s" mml))))

(define parse-note
  (let* ((basic-note ($or ($string "c")
                          ($string "d")
                          ($string "e")
                          ($string "f")
                          ($string "g")
                          ($string "a")
                          ($string "b")
                          ($string "r")))
         (note ($do (bn basic-note)
                    (qual ($optional ($or ($string "+") ($string "-"))))
                    ($return (string-append (rope->string bn) (or (rope->string qual) "")))))
         (chord ($many note))
         (len ($do (digits ($many ($one-of #[\d])))
                   (qual ($many ($string "+")))
                   ($return (cond
                              ((null? digits)
                               #f)
                              (else
                               digits (let1 basic-len (/ 1 (string->number (apply string digits)))
                                        (* basic-len (sum-ec (: i (+ (length qual) 1))
                                                             (/ 1 (expt 2 i))))))))))
         (chord+len ($do (ns chord)
                         (l ($optional len))
                         ($return (list ns l)))))
    (lambda (str)
      (call-with-input-string str
        (lambda (in)
          (begin0
            (peg-parse-port chord+len in)
            (unless (eof-object? (read-char in))
              (errorf "Invalid note: ~s" str))))))))

(define note->pitch
  (let1 table '(("c-" . -1)
                ("c" . 0)
                ("c+" . 1)
                ("d-" . 1)
                ("d" . 2)
                ("d+" . 3)
                ("e-" . 3)
                ("e" . 4)
                ("e+" . 5)
                ("f-" . 4)
                ("f" . 5)
                ("f+" . 6)
                ("g-" . 6)
                ("g" . 7)
                ("g+" . 8)
                ("a-" . 8)
                ("a" . 9)
                ("a+" . 10)
                ("b-" . 10)
                ("b" . 11)
                ("b+" . 12))
    (lambda (octave note)
      (if (equal? note "r")
          #f
          (+ (* 12 (+ octave 1)) (assoc-ref table note))))))

(define (note-spec->soundlet context str)
  (match-let1 ((notes ...) len) (parse-note str)
    (let ((sec (* (assoc-ref context 'tempo-factor 4)
                  (or len (assoc-ref context 'length (/ 1 4)))))
          (octave (assoc-ref context 'octave 4))
          (make-tone (assoc-ref context 'make-tone make-default-tone))
          (vols (stereo-volumes (assoc-ref context 'panning 0)))
          (amp-factor (assoc-ref context 'amp-factor 1.0))
          (envelope (assoc-ref context 'envelope default-envelope)))
      (receive (pitches _) (fold2 (lambda (note pitches prev-pitch)
                                    (let1 pitch-num (let1 p (note->pitch octave note)
                                                      (cond
                                                        ((not p)
                                                         #f)
                                                        ((<= prev-pitch p)
                                                         p)
                                                        (else
                                                         (+ p 12))))
                                      (values (cons pitch-num pitches)
                                              (or pitch-num prev-pitch))))
                                  '()
                                  (least-fixnum)
                                  notes)
        (compose-soundlets (map (lambda (pitch-num)
                                  (if pitch-num
                                      (make-tone (pitch pitch-num) amp-factor vols sec envelope)
                                      (make-silent sec)))
                                pitches))))))

(define (play-mml mml)
  (play-soundlet (compile-mml '() '() mml (lambda (context seq)
                                            (merge-soundlet seq)))))

(define (beep :optional (freq 2000) (velocity 1.0) (len 0.1))
  (play-mml `((wave square ,freq ,velocity ,len))))

(define (save-mml filename mml)
  (let* ((soundlet (compile-mml '() '() mml (lambda (context seq)
                                              (merge-soundlet seq))))
         (wave-data (soundlet->wave-data soundlet)))
    (call-with-output-file filename
      (lambda (out)
        (pack "A4LA4A4LSSLLSSA4L"
              `("RIFF"                                             ;; RIFF header
                ,(+ 4 4 16 4 4 (* (s16vector-length wave-data) 2)) ;; filesize - 8
                "WAVE"                                             ;; WAVE header
                "fmt "                                             ;; fmt chunk
                16                                                 ;; chunk size
                1                                                  ;; PCM
                2                                                  ;; num of channel
                44100                                              ;; sampling rate
                ,(* 44100 2 2)                                     ;; byte rate
                4                                                  ;; block align
                16                                                 ;; bits per sample
                "data"                                             ;; data chunk
                ,(* (s16vector-length wave-data) 2))
              :output out)
        (write-uvector wave-data out)))))


;;;
;;; Music
;;;

(set! (setter music-volume) set-music-volume!)


;;;
;;; Sound
;;;

(set! (setter sound-volume) set-sound-volume!)
