;;;
;;; audio.scm - WebAudio API
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
  (use graviton.app)
  (use graviton.browser-objects)
  (use graviton.jsffi)

  (export <base-audio-context>
          <audio-context>
          <offline-audio-context>
          <audio-node>
          <analyser-node>
          <audio-buffer>
          <audio-scheduled-source-node>
          <audio-buffer-source-node>
          <audio-destination-node>
          <audio-listener>
          <audio-param>
          <biquad-filter-node>
          <channel-merger-node>
          <channel-splitter-node>
          <constant-source-node>
          <convolver-node>
          <delay-node>
          <dynamic-compressor-node>
          <gain-node>
          <iir-filter-node>
          <media-element-audio-source-node>
          <media-stream-audio-destination-node>
          <media-stream-audio-source-node>
          <offline-audio-completion-event>
          <oscillator-node>
          <panner-node>
          <periodic-wave>
          <stereo-panner-node>
          <wave-shaper-node>
          audio-context))

(select-module graviton.audio)

(import-js ("/_g/audio.mjs" :only (audioContext)))

(define-class <base-audio-context> (<event-target>)
  ((current-time :jsproperty "currentTime"
                 :read-only? #t)
   (destination :jsproperty "destination"
                :read-only? #t
                :cacheable? #t)
   (listener :jsproperty "listener"
             :read-only? #t
             :cacheable? #t)
   (sample-rate :jsproperty "sampleRate"
                :read-only? #t
                :cacheable? #t)
   (state :jsproperty "state"
          :read-only? #t))
  :jsclass "BaseAudioContext")

(define-automatic-jsobject-methods <base-audio-context>
  ("createAnalyser" :result)
  ("createBiquadFilter" :result)
  ("createBuffer" :result)
  ("createBufferSource" :result)
  ("createChannelMerger" :result)
  ("createChannelSplitter" :result)
  ("createConvolver" :result)
  ("createDelay" :result)
  ("createDynamicCompressor" :result)
  ("createGain" :result)
  ("createIIRFilter" :result)
  ("createOscillator" :result)
  ("createPanner" :result)
  ("createPeriodicWave" :result)
  ("createWaveShaper" :result)
  "decodeAudioData")

(define-class <audio-context> (<base-audio-context>)
  ((base-latency :jsproperty "baseLatency")
   (output-latency :jsproperty "outputLatency"))
  :jsclass "AudioContext")

(define-automatic-jsobject-methods <audio-context>
  ("close" :result)
  ("createMediaElementSource" :result)
  ("createMediaStreamSource" :result)
  ("createMediaStreamDestination" :result)
  ("resume" :result)
  ("suspend" :result))

(define-class <offline-audio-context> (<base-audio-context>)
  ((length :jsproperty "length"
           :read-only? #t))
  :jsclass "OfflineAudioContext")

(define-automatic-jsobject-methods <offline-audio-context>
  ("suspend" :result)
  "startRendering")

(define-class <audio-node> (<event-target>)
  ((context :jsproperty "context"
            :read-only? #t)
   (number-of-inputs :jsproperty "numberOfInputs"
                     :read-only? #t)
   (number-of-outputs :jsproperty "numberOfOutputs"
                      :read-only? #t)
   (channel-count :jsproperty "channelCount")
   (channel-count-mode :jsproperty "channelCountMode")
   (channel-interpretation :jsproperty "channelInterpretation"))
  :jsclass "AudioNode")

(define-automatic-jsobject-methods <audio-node>
  ("connect" :result)
  "disconnect")

(define-class <analyser-node> (<audio-node>)
  ((fft-size :jsproperty "fftSize")
   (frequency-bin-count :jsproperty "frequencyBinCount"
                        :read-only? #t)
   (min-decibels :jsproperty "minDecibels")
   (max-decibels :jsproperty "maxDecibels")
   (smoothing-time-constant :jsproperty "smoothingTimeConstant"))
  :jsclass "AnalyserNode")

(define-automatic-jsobject-methods <analyser-node>
  ("getFloatFrequencyData" :result)
  ("getByteFrequencyData" :result)
  ("getFloatTimeDomainData" :result)
  ("getByteTimeDomainData" :result))

(define-class <audio-buffer> (<jsobject>)
  ((sample-rate :jsproperty "sampleRate"
                :read-only? #t)
   (length :jsproperty "length"
           :read-only? #t)
   (duration :jsproperty "duration"
             :read-only? #t)
   (number-of-channels :jsproperty "numberOfChannels"
                       :read-only? #t))
  :jsclass "AudioBuffer")

(define-automatic-jsobject-methods <audio-buffer>
  ("getChannelData" :result)
  "copyFromChannel")

(define-class <audio-scheduled-source-node> (<audio-node>)
  ()
  :jsclass "AudioScheduledSourceNode")

(define-automatic-jsobject-methods <audio-scheduled-source-node>
  "start"
  "stop")

(define-class <audio-buffer-source-node> (<audio-scheduled-source-node>)
  ((buffer :jsproperty "buffer")
   (detune :jsproperty "detune")
   (loop :jsproperty "loop")
   (loop-start :jsproperty "loopStart")
   (loop-end :jsproperty "loopEnd")
   (playback-rate :jsproperty "playbackRate"))
  :jsclass "AudioBufferSourceNode")

(define-class <audio-destination-node> (<audio-node>)
  ((max-channel-count :jsproperty "maxChannelCount"))
  :jsclass "AudioDestinationNode")

(define-class <audio-listener> (<jsobject>)
  ((position-x :jsproperty "positionX")
   (position-y :jsproperty "positionY")
   (position-z :jsproperty "positionZ")
   (forward-x :jsproperty "forwardX")
   (forward-y :jsproperty "forwardY")
   (forward-z :jsproperty "forwardZ")
   (up-x :jsproperty "upX")
   (up-y :jsproperty "upY")
   (up-z :jsproperty "upZ"))
  :jsclass "AudioListener")

(define-automatic-jsobject-methods <audio-listener>
  "setOrientation"
  "setPosition")


(define-class <audio-param> (<jsobject>)
  ((default-value :jsproperty "defaultValue"
     :read-only? #t)
   (max-value :jsproperty "maxValue"
              :read-only? #t)
   (min-value :jsproperty "minValue"
              :read-only? #t)
   (value :jsproperty "value"))
  :jsclass "AudioParam")

(define-automatic-jsobject-methods <audio-param>
  ("setValueAtTime" :result)
  ("linearRampToValueAtTime" :result)
  ("exponentialRampToValueAtTime" :result)
  ("setTargetAtTime" :result)
  ("setValueCurveAtTime" :result)
  ("cancelScheduledValues" :result)
  ("cancelAndHoldAtTime" :result))

(define-class <biquad-filter-node> (<audio-node>)
  ((frequency :jsproperty "frequency"
              :read-only? #t)
   (detune :jsproperty "detune"
           :read-only? #t)
   (q :jsproperty "Q"
      :read-only? #t)
   (gain :jsproperty "gain"
         :read-only? #t)
   (type :jsproperty "type"))
  :jsclass "BiquadFilterNode")

(define-jsobject-method <biquad-filter-node> get-frequency-response (frequency-array)
  (jslet/await ((self::object self)
                (frequency-array))
    (let ((mag-response-output (make Float32Array frequency-array.length))
          (phase-response-output (make Float32Array frequency-array.length)))
      (self.getFrequencyResponse mag-response-output phase-response-output)
      (result mag-response-output phase-response-output))))

(define-class <channel-merger-node> (<audio-node>)
  ()
  :jsclass "ChannelMergerNode")

(define-class <channel-splitter-node> (<audio-node>)
  ()
  :jsclass "ChannelSplitterNode")

(define-class <constant-source-node> (<audio-scheduled-source-node>)
  ((offset :jsproperty "offset"))
  :jsclass "ConstantSourceNode")

(define-class <convolver-node> (<audio-node>)
  ((buffer :jsproperty "buffer")
   (normalize :jsproperty "normalize"))
  :jsclass "ConvolverNode")

(define-class <delay-node> (<audio-node>)
  ((delay-time :jsproperty "delayTime"
               :read-only? #t))
  :jsclass "DelayNode")

(define-class <dynamic-compressor-node> (<audio-node>)
  ((threshold :jsproperty "threshold"
              :read-only? #t)
   (knee :jsproperty "knee"
         :read-only? #t)
   (ratio :jsproperty "ratio"
          :read-only? #t)
   (reduction :jsproperty "reduction"
              :read-only? #t)
   (attack :jsproperty "attack"
           :read-only? #t)
   (release :jsproperty "release"
            :read-only? #t))
  :jsclass "DynamicCompressorNode")

(define-class <gain-node> (<audio-node>)
  ((gain :jsproperty "gain"
         :read-only? #t))
  :jsclass "GainNode")

(define-class <iir-filter-node> (<audio-node>)
  ()
  :jsclass "IIRFilterNode")

(define-jsobject-method <iir-filter-node> get-frequency-response (frequency-array)
  (jslet/await ((self::object self)
                (frequency-array))
    (let ((mag-response-output (make Float32Array frequency-array.length))
          (phase-response-output (make Float32Array frequency-array.length)))
      (self.getFrequencyResponse mag-response-output phase-response-output)
      (result mag-response-output phase-response-output))))

(define-class <media-element-audio-source-node> (<audio-node>)
  ((media-element :jsproperty "mediaElement"
                  :read-only? #t))
  :jsclass "MediaElementAudioSourceNode")

(define-class <media-stream-audio-destination-node> (<audio-node>)
  ((stream :jsproperty "stream"))
  :jsclass "MediaStreamAudioDestinationNode")

(define-class <media-stream-audio-source-node> (<audio-node>)
  ((media-stream :jsproperty "mediaStream"
                 :read-only? #t))
  :jsclass "MediaStreamAudioSourceNode")

(define-class <offline-audio-completion-event> (<jsobject>)
  ((rendered-buffer :jsproperty "renderedBuffer"
                    :read-only? #t))
  :jsclass "OfflineAudioCompletionEvent")

(define-class <oscillator-node> (<audio-node>)
  ((frequency :jsproperty "frequency")
   (detune :jsproperty "detune")
   (type :jsproperty "type"))
  :jsclass "OscillatorNode")

(define-automatic-jsobject-methods <oscillator-node>
  "setPeriodicWave"
  "start"
  "stop")

(define-class <panner-node> (<audio-node>)
  ((cone-inner-angle :jsproperty "coneInnerAngle")
   (cone-outer-angle :jsproperty "coneOuterAngle")
   (cone-outer-gain :jsproperty "coneOuterGain")
   (distance-model :jsproperty "distanceModel")
   (max-distance :jsproperty "maxDistance")
   (orientation-x :jsproperty "orientationX")
   (orientation-y :jsproperty "orientationY")
   (orientation-z :jsproperty "orientationZ")
   (panning-model :jsproperty "panningModel")
   (position-x :jsproperty "positionX")
   (position-y :jsproperty "positionY")
   (position-z :jsproperty "positionZ")
   (ref-distance :jsproperty "refDistance")
   (rolloff-factor :jsproperty "rolloffFactor"))
  :jsclass "PannerNode")

(define-automatic-jsobject-methods <panner-node>
  "setPosition"
  "setOrientation")

(define-class <periodic-wave> (<jsobject>)
  ()
  :jsclass "PeriodicWave")

(define-class <stereo-panner-node> (<audio-node>)
  ((pan :jsproperty "pan"
        :read-only? #t))
  :jsclass "StereoPannerNode")

(define-class <wave-shaper-node> (<audio-node>)
  ((curve :jsproperty "curve")
   (oversample :jsproperty "oversample"))
  :jsclass "WaveShaperNode")

(define-global-jsobject audio-context (jslet/await ()
                                        (result audioContext)))
