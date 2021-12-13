;;;
;;; media-stream.scm - Media Stream API
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

(define-module graviton.media-stream
  (use graviton.browser-objects)

  (export <blob-event>
          <media-stream-track>
          <canvas-capture-media-stream-track>
          <media-devices>
          <media-stream>
          <media-stream-track-event>
          <media-recorder>
          media-recorder-is-type-supported?
          ))

(select-module graviton.media-stream)

(define-class <blob-event> (<event>)
  ((data :jsproperty "data"
         :read-only? #t)
   (timecode :jsproperty "timecode"
             :read-only? #t))
  :jsclass "BlobEvent")

(define-class <media-stream-track> (<jsobject>)
  ((content-hint :jsproperty "contentHint")
   (enabled :jsproperty "enabled")
   (id :jsproperty "id"
       :read-only? #t)
   (isolated :jsproperty "isolated"
             :read-only? #t)
   (kind :jsproperty "kind"
         :read-only? #t)
   (label :jsproperty "label"
          :read-only? #t)
   (muted :jsproperty "muted"
          :read-only? #t)
   (readonly :jsproperty "readonly"
             :read-only? #t)
   (ready-state :jsproperty "readyState"
                :read-only? #t))
  :jsclass "MediaStreamTrack")

(define-automatic-jsobject-methods <media-stream-track>
  ("applyConstraints" :result)
  ("clone" :result)
  ("getCapabilities" :result)
  ("getConstraints" :result)
  ("getSettings" :result)
  "stop")

(define-class <canvas-capture-media-stream-track> (<media-stream-track>)
  ((canvas :jsproperty "canvas"
           :read-only? #t
           :cacheable? #t))
  :jsclass "CanvasCaptureMediaStreamTrack")

(define-automatic-jsobject-methods <canvas-capture-media-stream-track>
  "requestFrame"


(define-class <media-devices> (<event-target>)
  ()
  :jsclass "MediaDevices")

(define-automatic-jsobject-methods <media-devices>
  ("emulateDevices" :result)
  ("getSupportedConstraints" :result)
  ("getDisplayMedia" :result)
  ("getUserMedia" :result))


(define-class <media-stream> (<jsobject>)
  ((active :jsproperty "active"
           :read-only? #t)
   (ended :jsproperty "ended"
          :read-only? #t)
   (id :jsproperty "id"
       :read-only? #t))
  :jsclass "MediaStream")

(define-automatic-jsobject-methods <media-stream>
  "addTrack"
  ("clone" :result)
  ("getAudioTracks" :result)
  ("getTrackById" :result)
  ("getTracks" :result)
  ("getVideoTracks" :result)
  "removeTrack")


(define-class <media-stream-track-event> (<event>)
  ((track :jsproperty "track"
          :read-only? #t))
  :jsclass "MediaStreamTrackEvent")


(define-class <media-recorder> (<event-target>)
  ((mime-type :jsproperty "mimeType"
              :read-only? #t)
   (state :jsproperty "state"
          :read-only? #t)
   (stream :jsproperty "state"
           :read-only? #t)
   (ignore-muted-media :jsproperty "ignoreMutedMedia"
                       :read-only? #t)
   (video-bits-per-second :jsproperty "videoBitsPerSecond"
                          :read-only? #t)
   (audio-bits-per-second :jsproperty "audioBitsPerSecond"
                          :read-only? #t))
  :jsclass "MediaRecorder")

(define-automatic-jsobject-methods <media-recorder>
  "pause"
  "requestData"
  "resume"
  "start"
  "stop")

(define (media-recorder-is-type-supported? mime-type)
  (jslet/await ((mime-type::string))
    (result (MediaRecorder.isTypeSupported mime-type))))
