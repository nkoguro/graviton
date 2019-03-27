;;;
;;; sound.scm - Sound
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

(inline-stub
 (define-cfn finalize-sound (z data::void*)
   ::void
   (when (GRV_SOUND_P z)
     (Mix_FreeChunk (-> (GRV_SOUND_PTR z) chunk))))

 (define-cfn set-playing-sound-context! (channel::int sound-context::GrvSoundContext*)
   ::GrvSoundContext*
   (lock-global-var)
   (let* ((prev::GrvSoundContext* (aref playing-sound-contexts channel)))
     (set! (aref playing-sound-contexts channel) sound-context)
     (unlock-global-var)
     (return prev)))

 (define-cfn find-available-channel ()
   ::int
   (let* ((i::int))
     (lock-global-var)
     (for ((set! i 0) (< i CHANNEL_SIZE) (inc! i))
       (when (== (aref playing-sound-contexts i) NULL)
         (break)))
     (unlock-global-var)

     (cond
       ((== i CHANNEL_SIZE)
        (return -1))
       (else
        (return i)))))

 (define-cfn stop-sound (channel::int)
   ::void
   (let* ((sound-context::GrvSoundContext* (set-playing-sound-context! channel NULL)))
     (when sound-context
       (set-future-result! (GRV_FUTURE_PTR (-> sound-context future)) 'stopped false))
     (Mix_HaltChannel channel)))

 (define-cfn finish-sound (channel::int)
   ::void :static
   (let* ((sound-context::GrvSoundContext* (set-playing-sound-context! channel NULL)))
     (when sound-context
       (set-future-result! (GRV_FUTURE_PTR (-> sound-context future)) 'finished false))))
 ) ;; end of inline-stub

(define-cproc load-sound (filename::<const-cstring>)
  (let* ((chunk::Mix_Chunk* (Mix_LoadWAV filename)))
    (unless chunk
      (Scm_Error "Mix_LoadWAV failed: %s" (Mix_GetError)))
    (let* ((gsound::GrvSound* (SCM_NEW (.type GrvSound)))
           (sound (MAKE_GRV_SOUND gsound)))
      (set! (-> gsound chunk) chunk)
      (Scm_RegisterFinalizer sound finalize-sound NULL)
      (return sound))))

(define-cproc play-sound (sound :key (channel #f))
  ::(<top> <int>)
  (unless (GRV_SOUND_P sound)
    (Scm_Error "<graviton-sound> required, but got %S" sound))

  (let* ((which::int))
    (cond
      ((SCM_INTP channel)
       (set! which (SCM_INT_VALUE channel))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which)))
      ((SCM_FALSEP channel)
       (set! which (find-available-channel))
       (when (< which 0)
         (Scm_Error "no available channels")))
      (else
       (Scm_Error "<integer> or #f required, but got %S" channel)))

    (stop-sound which)
    (let* ((sound-context::GrvSoundContext* (SCM_NEW (.type GrvSoundContext))))
      (set! (-> sound-context sound) sound
            (-> sound-context future) (make-future))
      (set-playing-sound-context! which sound-context)
      (when (< (Mix_PlayChannel which (-> (GRV_SOUND_PTR sound) chunk) 0) 0)
        (set-playing-sound-context! which NULL)
        (Scm_Error "Mix_PlayChannel failed: %s" (Mix_GetError)))
      (return (-> sound-context future) which))))

(define-cproc stop-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (let* ((i::int))
       (for ((set! i 0) (< i CHANNEL_SIZE) (inc! i))
         (stop-sound i))))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (stop-sound which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc playing-sound? (:optional (channel #f))
  ::<boolean>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Playing -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Playing which))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc pause-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (Mix_Pause -1))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Pause which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc resume-sound (:optional (channel #f))
  ::<void>
  (cond
    ((SCM_FALSEP channel)
     (Mix_Resume -1))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Resume which)))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc paused-sound? (:optional (channel #f))
  ::<boolean>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Paused -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Paused which))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(define-cproc set-sound-volume! (arg0 :optional arg1)
  ::<void>
  (cond
    ((and (SCM_INTP arg0)
          (SCM_UNBOUNDP arg1))
     ;; arg0: volume
     (Mix_Volume -1 (SCM_INT_VALUE arg0)))
    ((and (SCM_FALSEP arg0)
          (SCM_INTP arg1))
     ;; arg0: #f, arg1: volume
     (Mix_Volume -1 (SCM_INT_VALUE arg0)))
    ((and (SCM_INTP arg0)
          (SCM_INTP arg1))
     ;; arg0: channel, arg1: volume
     (let* ((which::int (SCM_INT_VALUE arg0)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (Mix_Volume which (SCM_INT_VALUE arg1))))
    ((SCM_UNBOUNDP arg1)
     (Scm_Error "invalid argument: %S" arg0))
    (else
     (Scm_Error "invalid arguments: %S %S" arg0 arg1))))

(define-cproc sound-volume (:optional (channel #f))
  ::<int>
  (cond
    ((SCM_FALSEP channel)
     (return (Mix_Volume -1 -1)))
    ((SCM_INTP channel)
     (let* ((which::int (SCM_INT_VALUE channel)))
       (unless (and (<= 0 which) (< which CHANNEL_SIZE))
         (Scm_Error "channel out of range: %d" which))
       (return (Mix_Volume which -1))))
    (else
     (Scm_Error "<integer> or #f required, but got %S" channel))))

(set! (setter sound-volume) set-sound-volume!)