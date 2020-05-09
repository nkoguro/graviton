;;;
;;; audio.scm - Graviton Audio Library
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

(define-module graviton.audio
  (use file.util)
  (use gauche.uvector)
  (use graviton)
  (use graviton.config)
  (use graviton.jsise)

  (export load-audio
          play-audio
          pause-audio
          play-wave
          play-rest
          load-pcm
          play-pcm))

(select-module graviton.audio)

(import-js ("graviton/audio.mjs" :only (audioContext audioChannels)))

(define-jsenum oscillator-type-enum
  (sine "sine")
  (square "square")
  (sawtooth "sawtooth")
  (triangle "triangle"))

(define-class <audio-media-element-node> (<proxy-object>)
  ((duration :init-value #f)))

(define (load-audio filename :key (content-type #f))
  (let* ((node (make <audio-media-element-node>))
         (node-id (proxy-object-id node))
         (url (resource-url filename :content-type content-type)))
    (jslet/result:then (lambda (duration)
                         (slot-set! node 'duration duration)
                         node)
        (node-id::u32
         url::string)
      (let ((audio (make Audio url)))
        (set! audio.onloadedmetadata (lambda ()
                                       (let ((source-node (audioContext.createMediaElementSource audio)))
                                         (Graviton.linkProxyObject node-id source-node)
                                         (source-node.connect audioContext.destination)
                                         (result audio.duration))))
        (set! audio.onstalled (lambda ()
                                (result-error "Load audio failed.")))))))

(define (play-audio audio)
  (jslet (audio::proxy)
    (audio.mediaElement.play)))

(define (pause-audio audio)
  (jslet (audio::proxy)
    (audio.mediaElement.pause)))

(define-class <audio-buffer> (<proxy-object>)
  ((sample-rate)
   (length)
   (duration)
   (number-of-channels)))

(define (load-pcm filename :key (content-type #f))
  (let* ((pcm (make <audio-buffer>))
         (object-id (proxy-object-id pcm))
         (url (resource-url filename :content-type content-type)))
    (jslet/result:then (lambda (sample-rate len duration num-of-channels)
                         (slot-set! pcm 'sample-rate sample-rate)
                         (slot-set! pcm 'length len)
                         (slot-set! pcm 'duration duration)
                         (slot-set! pcm 'number-of-channels num-of-channels)
                         pcm)
        (object-id::u32
         url::string)
      (let ((req (make XMLHttpRequest)))
        (req.open "GET" url #t)
        (set! req.responseType "arraybuffer")
        (set! req.onload (lambda ()
                           (cond
                             ((equal? req.status 200)
                              (let ((buf req.response))
                                ((ref (audioContext.decodeAudioData buf) then)
                                 (lambda (decoded-data)
                                   (Graviton.linkProxyObject object-id decoded-data)
                                   (result decoded-data.sampleRate
                                           decoded-data.length
                                           decoded-data.duration
                                           decoded-data.numberOfChannels))
                                 (lambda (reason)
                                   (result-error reason)))))
                             (else
                              (result-error (+ "Load PCM failed. (status:" req.status ")"))))))
        (req.send)))))

(define-method play-wave (channel-num type (freq <real>) len :optional (gain 1.0))
  (play-wave channel-num type (f64vector freq) len gain))

(define-method play-wave (channel-num type (freqs <f64vector>) len :optional (gain 1.0))
  (jslet (channel-num::u8
          type::oscillator-type-enum
          freqs::f64vector
          len::f64
          gain::f64)
    (let ((channel (aref audioChannels channel-num))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (dotimes (i freqs.length)
        (let ((oscillator-node (make OscillatorNode audioContext))
              (gain-node (audioContext.createGain)))
          (set! oscillator-node.type type)
          (set! oscillator-node.frequency.value (aref freqs i))
          (set! gain-node.gain.value gain)
          (oscillator-node.connect gain-node)
          (gain-node.connect audioContext.destination)
          (oscillator-node.start start-time)
          (oscillator-node.stop end-time)))
      (set! channel.lastPlaySec end-time))))

(define (play-rest channel-num len)
  (jslet (channel-num::u8
          len::f64)
    (let ((channel (aref audioChannels channel-num))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (set! channel.lastPlaySec end-time))))

(define (play-pcm channel-num pcm-data :optional (detune 0) (playback-rate 1.0) (loop-range #f) (len #f) (gain 1.0))
  (let ((loop? (if loop-range #t #f))
        (loop-start (if loop-range (list-ref loop-range 0) 0))
        (loop-end (if loop-range (list-ref loop-range 1) 0))
        (len (or len
                 (slot-ref pcm-data 'duration))))
  (jslet (channel-num::u8
          pcm-data::proxy
          detune::f64
          playback-rate::f64
          loop?::boolean
          loop-start::f64
          loop-end::f64
          len::f64
          gain::f64)
    (let ((channel (aref audioChannels channel-num))
          (pcm-node (audioContext.createBufferSource))
          (gain-node (audioContext.createGain))
          (start-time (Math.max channel.lastPlaySec audioContext.currentTime))
          (end-time (+ start-time len)))
      (set! pcm-node.buffer pcm-data)
      (unless (equal? detune 0)
        (set! pcm-node.detune.value detune))
      (unless (equal? playback-rate 1.0)
        (set! pcm-node.playbackRate.value playback-rate))
      (when loop?
        (set! pcm-node.loop loop?)
        (set! pcm-node.loopStart loop-start)
        (set! pcm-node.loopEnd loop-end))
      (set! gain-node.gain.value gain)
      (pcm-node.connect gain-node)
      (gain-node.connect audioContext.destination)
      (pcm-node.start start-time)
      (pcm-node.stop end-time)
      (set! channel.lastPlaySec end-time)))))
