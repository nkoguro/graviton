;;;
;;; audio.scm - Graviton Utility Toolkit Audio
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

(define-module graviton.grut.audio
  (use gauche.collection)
  (use gauche.sequence)
  (use gauche.uvector)
  (use graviton.async)
  (use graviton.jsffi)
  (use parser.peg)
  (use srfi-1)
  (use srfi-13)
  (use util.match)

  (export play-mml
          resume-track
          resume-all-tracks
          pause-track
          pause-all-tracks
          stop-track
          stop-all-tracks
          wait-track
          wait-all-tracks

          play-beep))

(select-module graviton.grut.audio)

(import-js ("/_g/grut/audio.mjs" :only (soundTrackManager)))

(define (enqueue-sound track sound-data-list)
  (jslet ((track::string)
          (sound-data-list::array))
    (soundTrackManager.enqueue track sound-data-list)))

(define (resume-track :rest tracks)
  (jslet ((tracks::list (map keyword->string tracks)))
    (soundTrackManager.start tracks)))

(define (resume-all-tracks)
  (jslet ()
    (soundTrackManager.startAll)))

(define (pause-track :rest tracks)
  (jslet ((tracks::list (map keyword->string tracks)))
    (soundTrackManager.pause tracks)))

(define (pause-all-tracks)
  (jslet ()
    (soundTrackManager.pauseAll)))

(define (stop-track :rest tracks)
  (jslet ((tracks::list (map keyword->string tracks)))
    (soundTrackManager.stop tracks)))

(define (stop-all-tracks)
  (jslet ()
    (soundTrackManager.stopAll)))

(define (wait-track :rest tracks)
  (shift-callback* cont-callback
    (jslet ((tracks::list (map keyword->string tracks))
            (cont-callback))
      (soundTrackManager.waitCompletion tracks cont-callback))))

(define (wait-all-tracks)
  (shift-callback* cont-callback
    (jslet ((cont-callback))
      (soundTrackManager.waitAllCompletion cont-callback))))

(define-constant OSCILLATOR-SOUND 1)
(define-constant COMPOSED-SOUND 2)
(define-constant NOISE-SOUND 3)
(define-constant REST-SOUND 5)
(define-constant SET-CUSTOM-WAVE 6)

(define-constant SINE-WAVE 1)
(define-constant SQUARE-WAVE 2)
(define-constant SAWTOOTH-WAVE 3)
(define-constant TRIANGLE-WAVE 4)
(define-constant CUSTOM-WAVE 5)

(define-constant VALUE-AT-TIME-PARAM 1)
(define-constant LINEAR-RAMP-TO-VALUE-AT-TIME-PARAM 2)
(define-constant EXPONENTIAL-RAMP-TO-VALUE-AT-TIME-PARAM 3)
(define-constant TARGET-AT-TIME-PARAM 4)
(define-constant VALUE-CURVE-AT-TIME-PARAM 5)
(define-constant OSCILLATOR-PARAM 6)

(define (parse-control spec :optional (converter values))
  (let loop ((spec spec)
             (data '()))
    (match spec
      ((? real? v)
       (converter v))
      (()
       (list->vector (reverse data)))
      ((':set-value-at-time (v t) rest ...)
       (loop rest (cons (vector VALUE-AT-TIME-PARAM (converter v) t) data)))
      ((':linear-ramp-to-value-at-time (v t) rest ...)
       (loop rest (cons (vector LINEAR-RAMP-TO-VALUE-AT-TIME-PARAM (converter v) t) data)))
      ((':exponential-ramp-to-value-at-time (v t) rest ...)
       (loop rest (cons (vector EXPONENTIAL-RAMP-TO-VALUE-AT-TIME-PARAM (converter v) t) data)))
      ((':set-target-at-time (v t c) rest ...)
       (loop rest (cons (vector TARGET-AT-TIME-PARAM (converter v) t c) data)))
      ((':set-curve-at-time (vs t duration) rest ...)
       (loop rest (cons (vector VALUE-CURVE-AT-TIME-PARAM (map converter vs) t duration) data)))
      ((':oscillator (freq-spec wave-form-spec detune-spec delay depth-spec) rest ...)
       (loop rest (cons (vector OSCILLATOR-PARAM
                                (parse-control freq-spec)
                                (parse-wave-form wave-form-spec)
                                (parse-control detune-spec)
                                delay
                                (parse-control depth-spec))
                        data)))
      (_
       (errorf "malformed control spec: ~s" spec)))))

(define (parse-wave-form spec)
  (match spec
    ('sine
     SINE-WAVE)
    ('square
      SQUARE-WAVE)
    ('sawtooth
      SAWTOOTH-WAVE)
    ('triangle
      TRIANGLE-WAVE)
    ('custom
     CUSTOM-WAVE)
    (((? f32vector real) (? f32vector imag))
     (vector real imag))
    (_
     (errorf "malformed wave-form spec: ~s" spec))))

(define %number
  (let ((%sign ($->rope ($. #[+-])))
        (%digits ($->rope ($many ($. #[\d]) 1)))
        (%frac ($->rope ($. #\.) ($many ($. #[\d]) 1))))
    ($lift ($ string->number $ string-concatenate
              $ map rope->string $ list $*)
           ($optional %sign) %digits ($optional %frac))))

(define %note
  (let* ((stem->number (lambda (rope)
                         (assoc-ref '(("c" . 0)
                                      ("d" . 2)
                                      ("e" . 4)
                                      ("f" . 5)
                                      ("g" . 7)
                                      ("a" . 9)
                                      ("b" . 11))
                                    (rope->string rope))))
         (accidental->number (lambda (rope)
                               (assoc-ref '(("+" . 1)
                                            ("-" . -1))
                                          (rope->string rope))))
         (%stem ($lift stem->number ($. #[a-g])))
         (%accidental ($lift accidental->number ($. #[+\-])))
         (%note-base-length ($lift (^x (/. 1 (string->number x))) ($->string ($many ($. #[\d]) 1))))
         (%dots ($lift length ($many ($. "."))))
         (%note-length ($lift (^(x n) (* x (- 2 (expt 0.5 n)))) %note-base-length %dots))
         (%note-total-length ($lift (cut apply + <> <>) %note-length ($many ($seq ($. #[+^]) %note-length))))
         (%slur ($or ($->symbol ($. "&")) ($->symbol ($. "^"))))
         (%rest ($->symbol ($. "r")))
         (%noise ($->symbol ($. "x")))
         (%tempo ($->symbol ($. "t")))
         (%length ($->symbol ($. "l")))
         (%octave ($->symbol ($. "o")))
         (%octave++ ($->symbol ($. ">")))
         (%octave-- ($->symbol ($. "<")))
         (%volume ($->symbol ($. "v")))
         (%pan ($->symbol ($. "p")))
         (%gate ($->symbol ($. "q")))
         (%note1 ($lift (^(x y) (if y (+ x y) x)) %stem ($optional %accidental))))
    ($or ($lift (lambda (rels+len-list eos)
                  (cons 'note-seq (map (match-lambda
                                         ((rels len)
                                          (list (values-ref (map-accum (lambda (rel prev)
                                                                         (let1 r (if (<= rel prev)
                                                                                   (+ rel 12)
                                                                                   rel)
                                                                           (values r r)))
                                                                       -1
                                                                       rels)
                                                            0)
                                                len)))
                                       rels+len-list)))
                ($sep-by ($list ($many %note1 1) ($optional %note-total-length)) %slur) ($eos))
         ($lift list %rest ($optional %note-total-length) ($eos))
         ($lift list %noise ($optional %note-total-length) ($eos))
         ($lift list %tempo %number ($eos))
         ($lift list %length %number ($eos))
         ($lift list %octave %number ($eos))
         ($lift list %octave++ ($eos))
         ($lift list %octave-- ($eos))
         ($lift list %pan %number)
         ($lift list %gate %number)
         ($lift list %volume %number ($eos)))))

(define (parse-note env note)
  (let* ((octave (assq-ref env 'octave 4))
         (note-offset (* (+ octave 1) 12))
         (length-unit (/. (* 60 4) (assq-ref env 'tempo 120)))
         (default-mag (assq-ref env 'length-magnifier 0.25))
         (wave-form (assq-ref env 'wave-form 'sine))
         (volume (assq-ref env 'volume 0.5))
         (stereo-pan (assq-ref env 'stereo-pan 0))
         (envelope (assq-ref env 'envelope (lambda (vol len) vol)))
         (release (assq-ref env 'release 0))
         (detune (assq-ref env 'detune 0))
         (gate/step (assq-ref env 'gate/step (/. 7 8))))
    (match (peg-parse-string %note (symbol->string note))
      (('note-seq notes+len ...)
       (map-with-index (match-lambda*
                         ((i ((note-rels ...) len-mag))
                          (let* ((len (* length-unit (or len-mag default-mag)))
                                 (gate/step (if (= i (- (length notes+len) 1))
                                              gate/step
                                              1))
                                 (gate-len (* len gate/step)))
                            `(chord ,@(map (lambda (rel)
                                             `(note ,(+ note-offset rel)
                                                    ,wave-form
                                                    ,detune
                                                    ,gate-len
                                                    ,(- len gate-len)
                                                    ,release
                                                    ,(envelope volume gate-len)
                                                    ,stereo-pan))
                                           note-rels)))))
                       notes+len))
      (('r len-mag _)
       `((rest ,(* length-unit (or len-mag default-mag)))))
      (('x len-mag _)
       (let* ((len (* length-unit (or len-mag default-mag)))
              (gate-len (* len gate/step)))
         `((noise ,gate-len ,(- len gate-len) ,release ,(envelope volume len) ,stereo-pan))))
      (('t tempo _)
       `(:tempo ,tempo))
      (('l len _)
       `(:length ,len))
      (('o octave _)
       `(:octave ,octave))
      (('v volume _)
       `(:volume ,volume))
      (('> _)
       `(:octave ,(+ octave 1)))
      (('< _)
       `(:octave ,(- octave 1)))
      (('p pan)
       `(:stereo-pan ,pan))
      (('q gate/step)
       `(:gate/step ,gate/step))
      (_
       (errorf "Invalid note: ~a" note)))))

(debug-print-width #f)

(define (parse-mml env expr)
  (let loop ((env '())
             (expr expr)
             (sound-data-list '()))
    (match expr
      (()
       (list->vector (reverse sound-data-list)))
      ((('oscillator freq-spec wave-form detune-spec len margin release depth-spec pan-spec) rest ...)
       (loop env
             rest
             (cons (vector OSCILLATOR-SOUND
                           (parse-control freq-spec)
                           (parse-wave-form wave-form)
                           (parse-control detune-spec)
                           len
                           margin
                           release
                           (parse-control depth-spec)
                           (parse-control pan-spec))
                   sound-data-list)))
      ((('note note-spec wave-form detune-spec len margin release depth-spec pan-spec) rest ...)
       (loop env
             rest
             (cons (vector OSCILLATOR-SOUND
                           (parse-control note-spec (^n (* 440 (expt 2 (/. (- n 69) 12)))))
                           (parse-wave-form wave-form)
                           (parse-control detune-spec)
                           len
                           margin
                           release
                           (parse-control depth-spec)
                           (parse-control pan-spec))
                   sound-data-list)))
      ((('noise len margin release depth-spec pan-spec) rest ...)
       (loop env
             rest
             (cons (vector NOISE-SOUND len margin release depth-spec pan-spec)
                   sound-data-list)))
      ((('chord (and soundlets ((or 'oscillator 'note 'noise) _ ...)) ...) rest ...)
       (loop env
             rest
             (cons (if (= (length soundlets) 1)
                     (vector-ref (parse-mml env soundlets) 0)
                     (vector-append (vector COMPOSED-SOUND) (parse-mml env soundlets)))
                   sound-data-list)))
      ((('rest len) rest ...)
       (loop env
             rest
             (cons (vector REST-SOUND len) sound-data-list)))
      ((('begin expr ...) rest ...)
       (loop env
             rest
             (append (reverse (vector->list (parse-mml env expr)))
                     sound-data-list)))

      ((':tempo tempo rest ...)
       (loop (assq-set! env 'tempo tempo)
             rest
             sound-data-list))
      ((':length len rest ...)
       (loop (assq-set! env 'length-magnifier (/. 1 len))
             rest
             sound-data-list))
      ((':octave octave rest ...)
       (loop (assq-set! env 'octave octave)
             rest
             sound-data-list))
      ((':volume volume rest ...)
       (loop (assq-set! env 'volume volume)
             rest
             sound-data-list))
      ((':stereo-pan pan rest ...)
       (loop (assq-set! env 'stereo-pan pan)
             rest
             sound-data-list))
      ((':wave-form ((? f32vector? real) (? f32vector? imag)) rest ...)
       (loop (assq-set! env 'wave-form 'custom)
             rest
             (cons (vector SET-CUSTOM-WAVE real imag)
                   sound-data-list)))
      ((':wave-form wave-form rest ...)
       (loop (assq-set! env 'wave-form wave-form)
             rest
             sound-data-list))
      ((':adsr (attack decay sustain release) rest ...)
       (loop (list* `(envelope . ,(lambda (vol len)
                                    `(:set-value-at-time (0 0)
                                      :linear-ramp-to-value-at-time (,vol ,attack)
                                      :linear-ramp-to-value-at-time (,(* sustain vol) ,(+ attack decay))
                                      :set-value-at-time (,(* sustain vol) ,len)
                                      :linear-ramp-to-value-at-time (0 ,(+ len release)))))
                    `(release . ,release)
                    env)
             rest
             sound-data-list))
      ((':adsr #f rest ...)
       (loop (alist-delete 'envelope env)
             rest
             sound-data-list))
      ((':detune d rest ...)
       (loop (assq-set! env 'detune d)
             rest
             sound-data-list))
      ((':gate/step v rest ...)
       (loop (assq-set! env 'gate/step v)
             rest
             sound-data-list))
      (((? (^x (and (symbol? x) (not (keyword? x)))) note) rest ...)
       (loop env
             (append (parse-note env note) rest)
             sound-data-list))
      (_
       (errorf "malformed mml: ~s" expr)))))

(define (enqueue-mml :rest args)
  (let loop ((args args))
    (match args
      (()
       #t)
      (((? keyword? track) mml rest ...)
       (enqueue-sound (keyword->string track) (parse-mml '() mml))
       (loop rest))
      (_
       (errorf "keyword-value list required, but got ~s" args)))))

(define (play-mml :rest args)
  (apply enqueue-mml args)
  (apply resume-track (filter keyword? args)))

(define (play-beep freq len :key (wave-form 'sine) (volume 1))
  (play-mml :beep `((oscillator ,freq ,wave-form 0 ,len 0 0 ,volume 0))))

