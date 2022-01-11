;;;
;;; speech.scm - Graviton Utility Toolkit Speech
;;;
;;;   Copyright (c) 2022 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.grut.speech
  (use graviton)
  (use srfi-1)
  (use srfi-13)

  (export <speech-synthesis-voice>
          query-all-voices
          query-voice
          speak
          pause-speech
          resume-speech
          cancel-speech))

(select-module graviton.grut.speech)

(import-js ("/_g/grut/speech.mjs" :as Speech))

;;;

(define-class <speech-synthesis-voice> ()
  ((%internal-data :init-keyword :%internal-data)
   (default :allocation :virtual :slot-ref (lambda (obj) (vector-ref (~ obj'%internal-data) 0)))
   (lang :allocation :virtual :slot-ref (lambda (obj) (vector-ref (~ obj'%internal-data) 1)))
   (local-service :allocation :virtual :slot-ref (lambda (obj) (vector-ref (~ obj'%internal-data) 2)))
   (name :allocation :virtual :slot-ref (lambda (obj) (vector-ref (~ obj'%internal-data) 3)))
   (voice-uri :allocation :virtual :slot-ref (lambda (obj) (vector-ref (~ obj'%internal-data) 4)))))

(define (query-all-voices :key lang name default local-service voice-uri (wait? #t))
  (define (check-string voice key pred)
    (cond
      ((undefined? pred)
       #t)
      ((string? pred)
       (string-contains (slot-ref voice key) pred))
      ((regexp? pred)
       (regexp-matches? pred (slot-ref voice key)))
      (else
       (errorf "<string> or <regexp> expected, but got ~s" pred))))
  (define (check-value voice key val)
    (cond
      ((undefined? val)
       #t)
      (else
       (equal? (slot-ref voice key) val))))
  (let loop ((voices (map (cut make <speech-synthesis-voice> :%internal-data <>)
                          (vector->list (jslet/await ((wait? wait?))
                                          (Speech.fetchAllVoices (lambda (voices)
                                                                 (respond voices))
                                                               wait?)))))
             (result '()))
    (cond
      ((null? voices)
       (reverse result))
      (else
       (let1 voice (car voices)
         (loop (cdr voices)
               (if (and (check-string voice 'lang lang)
                        (check-string voice 'name name)
                        (check-value voice 'default default)
                        (check-string voice 'local-service local-service)
                        (check-string voice 'voice-uri voice-uri))
                 (cons voice result)
                 result)))))))

(define (query-voice :key lang name default local-service voice-uri (wait? #t))
  (let1 voices (query-all-voices :lang lang :name name :default default :local-service local-service :voice-uri voice-uri :wait? wait?)
    (if (null? voices)
      #f
      (first voices))))

(define (speak text :key (voice #f))
  (jslet ((text text)
          (lang (and voice
                     (slot-ref voice 'lang)))
          (voiceName (and voice
                          (slot-ref voice 'name))))
    (Speech.speak text lang voiceName)))

(define (pause-speech)
  (jslet ()
    (window.speechSynthesis.cancel)))

(define (resume-speech)
  (jslet ()
    (window.speechSynthesis.resume)))

(define (cancel-speech)
  (jslet ()
    (window.speechSynthesis.cancel)))

