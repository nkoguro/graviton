;;;
;;; music.scm - PSG MML and Music
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

(select-module graviton.audio)

;;;
;;; MML
;;;

(inline-stub
 (declcode
  (.include "SDL.h"
            "SDL_mixer.h"
            "gauche.h"
            "graviton.h"
            "stdbool.h"
            "string.h"))

 (.define MML_MUSIC_CONTEXT_INITIAL_LENGTH 16)
 (.define NOISE_TABLE_SIZE 32768)

 (define-cvar Grv_MusicLastFinishedTick::Uint32)
 (define-cvar mml-music-context-queue::GrvMMLMusicContextQueue :static)
 (define-cvar mml-paused?::bool :static)
 (define-cvar playing-music-context::GrvMusicContext* :static)
 (define-cvar noise-table::double* :static)

 (initcode
  (set! (ref mml-music-context-queue buf) (SCM_NEW_ARRAY (.type GrvMMLMusicContext*) MML_MUSIC_CONTEXT_INITIAL_LENGTH)
        (ref mml-music-context-queue length) MML_MUSIC_CONTEXT_INITIAL_LENGTH
        (ref mml-music-context-queue start) 0
        (ref mml-music-context-queue end) 0
        mml-paused? false)
  (dotimes (i (ref mml-music-context-queue length))
    (set! (aref (ref mml-music-context-queue buf) i) NULL))

  (set! noise-table (SCM_NEW_ATOMIC_ARRAY (.type double) NOISE_TABLE_SIZE))
  (dotimes (i NOISE_TABLE_SIZE)
    (set! (aref noise-table i) (* (- (/ (cast double (random)) RAND_MAX) 0.5) 2.0)))

  (set! playing-music-context NULL))

 (define-cfn Grv_IsPlayingMML ()
   ::bool
   (return (!= (Mix_GetMusicHookData) NULL)))

 (define-cfn enqueue-mml-music-context! (gcontext::GrvMMLMusicContext*)
   ::void :static
   (Grv_LockGlobal)
   (set! (aref (ref mml-music-context-queue buf) (ref mml-music-context-queue end)) gcontext
         (ref mml-music-context-queue end) (% (+ (ref mml-music-context-queue end) 1)
                                              (ref mml-music-context-queue length)))
   (when (== (ref mml-music-context-queue start) (ref mml-music-context-queue end))
     (let* ((newlen::int (* (ref mml-music-context-queue length) 2))
            (newbuf::GrvMMLMusicContext** (SCM_NEW_ARRAY (.type GrvMMLMusicContext*) newlen)))
       (dotimes (i (ref mml-music-context-queue length))
         (set! (aref newbuf i) (aref (ref mml-music-context-queue buf) (% (+ i (ref mml-music-context-queue start))
                                                                          (ref mml-music-context-queue length)))))
       (set! (ref mml-music-context-queue start) 0
             (ref mml-music-context-queue end) (ref mml-music-context-queue length)
             (ref mml-music-context-queue buf) newbuf
             (ref mml-music-context-queue length) newlen)))
   (Grv_UnlockGlobal))

 (define-cfn dequeue-mml-music-context! ()
   ::GrvMMLMusicContext* :static
   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (Grv_LockGlobal)
     (unless (== (ref mml-music-context-queue start) (ref mml-music-context-queue end))
       (set! gcontext (aref (ref mml-music-context-queue buf) (ref mml-music-context-queue start))
             (ref mml-music-context-queue start) (% (+ (ref mml-music-context-queue start) 1)
                                                    (ref mml-music-context-queue length))))
     (Grv_UnlockGlobal)
     (return gcontext)))

 (define-cfn retain-soundlet! (gcontext::GrvMMLMusicContext* gsoundlet::GrvSoundlet* pos::int)
   ::void
   (unless gsoundlet
     (return))
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (when (== (aref (-> gcontext soundlet-contexts) i) NULL)
       (let* ((sctx:: GrvSoundletContext* (SCM_NEW (.type GrvSoundletContext))))
         (set! (-> sctx start-position) pos
               (-> sctx soundlet) gsoundlet
               (aref (-> gcontext soundlet-contexts) i) sctx))
       (when (== (-> gsoundlet type) SOUNDLET_COMPOSITE)
         (let* ((gcomposite::GrvCompositeSoundlet* (ref (-> gsoundlet data) composite)))
           (dotimes (j (-> gcomposite num-children))
             (retain-soundlet! gcontext (aref (-> gcomposite children) j) pos))))
       (return)))

   (let* ((new-size::int (* (-> gcontext num-soundlet-contexts) 2))
          (new-contexts::GrvSoundletContext** (SCM_NEW_ARRAY (.type GrvSoundletContext*) new-size)))
     (dotimes (i new-size)
       (cond
         ((< i (-> gcontext num-soundlet-contexts))
          (set! (aref new-contexts i) (aref (-> gcontext soundlet-contexts) i)))
         (else
          (set! (aref new-contexts i) NULL))))
     (set! (-> gcontext soundlet-contexts) new-contexts
           (-> gcontext num-soundlet-contexts) new-size)
     (retain-soundlet! gcontext gsoundlet pos)))

 (define-cfn release-soundlet! (gcontext::GrvMMLMusicContext* gsoundlet::GrvSoundlet*)
   ::void
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (let* ((sctx::GrvSoundletContext* (aref (-> gcontext soundlet-contexts) i)))
       (when (and sctx (== (-> sctx soundlet) gsoundlet))
         (set! (aref (-> gcontext soundlet-contexts) i) NULL)
         (return)))))

 (define-cfn inc-buffer! (buf::int16_t* index::int dv::double)
   ::void
   (let* ((v::int (+ (aref buf index) (cast int (floor (* dv INT16_MAX))))))
     (cond
       ((< INT16_MAX v)
        (set! (aref buf index) INT16_MAX))
       ((< v INT16_MIN)
        (set! (aref buf index) INT16_MIN))
       (else
        (set! (aref buf index) v)))))

 (define-cfn compute-tone-silent (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (return 0))

 (define-cfn compute-tone-sine (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (set! v (+ v (* (aref amps i) (sin (* 2 M_PI (aref freqs i) (/ rel-pos 44100.0)))))))
     (return v)))

 (define-cfn compute-tone-square50 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-square12 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len12::int (/ len 8)))
         (cond
           ((< (% rel-pos len) len12)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-square25 (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len25::int (/ len 4)))
         (cond
           ((< (% rel-pos len) len25)
            (set! v (+ v amp)))
           (else
            (set! v (- v amp))))))
     (return v)))

 (define-cfn compute-tone-triangle (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (half::int (/ len 2)))
         (cond
           ((< (% rel-pos len) half)
            (set! v (+ v (- (/ (* 2.0 amp (% rel-pos len)) half) amp))))
           (else
            (set! v (+ v (+ (/ (* -2.0 amp (% rel-pos len)) half) (* 3.0 amp))))))))
     (return v)))

 (define-cfn compute-tone-sawtooth (freqs::double* amps::double* num-freqs::int rel-pos::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq)))))
         (set! v (+ v (- (/ (* 2.0 amp (% rel-pos len)) len) amp)))))
     (return v)))

 (define-cfn compute-tone-long-noise (freqs::double* amps::double* num-freqs::int rel-pos::int dummy::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (when (< len50 1)
           (set! len50 1))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) NOISE_TABLE_SIZE))))))
           (else
            (set! v (- v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) NOISE_TABLE_SIZE)))))))))
     (return v)))

 (.define SHORT_NOISE_SIZE 100)
 (define-cfn compute-tone-short-noise (freqs::double* amps::double* num-freqs::int rel-pos::int dummy::int)
   ::double
   (let* ((v::double 0))
     (dotimes (i num-freqs)
       (let* ((freq::double (aref freqs i))
              (amp::double (aref amps i))
              (len::int (cast int (round (/ 44100.0 freq))))
              (len50::int (/ len 2)))
         (when (< len50 1)
           (set! len50 1))
         (cond
           ((< (% rel-pos len) len50)
            (set! v (+ v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) SHORT_NOISE_SIZE))))))
           (else
            (set! v (- v (* amp (aref noise-table (% (+ (/ rel-pos len50) dummy) SHORT_NOISE_SIZE)))))))))
     (return v)))

 (define-cfn render-tone (buf::int16_t* index::int gcontext::GrvMMLMusicContext* gsoundlet-context::GrvSoundletContext*)
   ::void
   (let* ((gsoundlet::GrvSoundlet* (-> gsoundlet-context soundlet))
          (gtone::GrvToneSoundlet* (ref (-> gsoundlet data) tone))
          (start-pos::int (-> gsoundlet-context start-position))
          (cur-pos::int (-> gcontext position))
          (rel-pos::int (- cur-pos start-pos))
          (length::int (-> gsoundlet length))
          (attack-time::int (-> gtone attack-time))
          (decay-time::int (-> gtone decay-time))
          (sustain-level::double (-> gtone sustain-level))
          (release-time::int (-> gtone release-time)))
     (when (< rel-pos 0)
       (return))
     (when (== rel-pos (- length 1))
       (retain-soundlet! gcontext (-> gsoundlet next) (+ cur-pos 1)))
     (when (<= (+ length release-time) rel-pos)
       (release-soundlet! gcontext (-> gsoundlet-context soundlet))
       (return))

     (let* ((v::double 0))
       (case (-> gtone type)
         ((TONE_SILENT)
          (set! v (compute-tone-silent (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SINE)
          (set! v (compute-tone-sine (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE50)
          (set! v (compute-tone-square50 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE12)
          (set! v (compute-tone-square12 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SQUARE25)
          (set! v (compute-tone-square25 (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_TRIANGLE)
          (set! v (compute-tone-triangle (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_SAWTOOTH)
          (set! v (compute-tone-sawtooth (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos)))
         ((TONE_LONG_NOISE)
          (set! v (compute-tone-long-noise (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos start-pos)))
         ((TONE_SHORT_NOISE)
          (set! v (compute-tone-short-noise (-> gtone freqs) (-> gtone amps) (-> gtone num-freqs) rel-pos start-pos))))
       (cond
         ((< rel-pos attack-time)
          (set! v (* (/ 1.0 attack-time) rel-pos v)))
         ((and (<= attack-time rel-pos) (< rel-pos (+ attack-time decay-time)))
          (set! v (* (+ (* (/ (- sustain-level 1.0) decay-time) (- rel-pos attack-time)) 1.0) v)))
         ((and (<= (+ attack-time decay-time) rel-pos) (< rel-pos length))
          (set! v (* sustain-level v)))
         (else
          (set! v (* (+ (* (/ (- sustain-level) release-time) (- rel-pos length)) sustain-level) v))))
       (inc-buffer! buf index (* (-> gtone left-volume) v))
       (inc-buffer! buf (+ index 1) (* (-> gtone right-volume) v)))))

 (define-cfn render-composite (buf::int16_t* index::int gcontext::GrvMMLMusicContext* gsoundlet-context::GrvSoundletContext*)
   ::void
   (let* ((gsoundlet::GrvSoundlet* (-> gsoundlet-context soundlet))
          (gcomposite::GrvCompositeSoundlet* (ref (-> gsoundlet data) composite))
          (start-pos::int (-> gsoundlet-context start-position))
          (cur-pos::int (-> gcontext position))
          (rel-pos::int (- cur-pos start-pos))
          (length::int (-> gsoundlet length)))
     (when (== rel-pos (- length 1))
       (retain-soundlet! gcontext (-> gsoundlet next) (+ cur-pos 1)))
     (when (<= length rel-pos)
       (release-soundlet! gcontext (-> gsoundlet-context soundlet)))))

 (define-cfn render-context (buf::int16_t* index::int gcontext::GrvMMLMusicContext*)
   ::void :static
   (dotimes (i (-> gcontext num-soundlet-contexts))
     (let* ((sctx::GrvSoundletContext* (aref (-> gcontext soundlet-contexts) i)))
       (unless (== sctx NULL)
         (cond
           ((== (-> sctx soundlet type) SOUNDLET_TONE)
            (render-tone buf index gcontext sctx))
           ((== (-> sctx soundlet type) SOUNDLET_COMPOSITE)
            (render-composite buf index gcontext sctx)))))))

 (define-cfn fill-audio-stream (udata::void* stream::Uint8* len::int)
   ::void :static
   (memset stream 0 len)

   (let* ((buf::int16_t* (cast int16_t* stream))
          (buf-length::int (/ len 2))
          (index::int 0)
          (gcontext::GrvMMLMusicContext* (cast GrvMMLMusicContext* udata))
          (pos::int (-> gcontext position))
          (pos-end::int (+ pos (/ buf-length 2)))
          (i::int 0))
     (when (and mml-paused?
                (GRV_FUTUREP (-> gcontext future)))
       (return))

     (while (< i buf-length)
       (render-context buf i gcontext)
       (inc! (-> gcontext position))
       (set! i (+ i 2)))

     (let* ((num::int 0))
       (dotimes (i (-> gcontext num-soundlet-contexts))
         (unless (== (aref (-> gcontext soundlet-contexts) i) NULL)
           (inc! num)))
       (when (and (== num 0)
                  (GRV_FUTUREP (-> gcontext future)))
         (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
           (Grv_SetFutureResult gfuture (SCM_LIST1 'finished) false))

         (let* ((next-gcontext::GrvMMLMusicContext* (dequeue-mml-music-context!)))
           (cond
             (next-gcontext
              (set! (-> gcontext position) (-> next-gcontext position)
                    (-> gcontext soundlet-contexts) (-> next-gcontext soundlet-contexts)
                    (-> gcontext num-soundlet-contexts) (-> next-gcontext num-soundlet-contexts)
                    (-> gcontext future) (-> next-gcontext future)))
             (else
              (GRV_SEND_EVENT GRV_EVENT_MML_FINISH NULL NULL))))))))

 (define-cfn stop-mml ()
   ::void
   (let* ((gcontext::GrvMMLMusicContext* (Mix_GetMusicHookData)))
     (when gcontext
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (Grv_SetFutureResult gfuture (SCM_LIST1 'stopped) false))))
   (Mix_HookMusic NULL NULL)

   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (while (= gcontext (dequeue-mml-music-context!))
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (Grv_SetFutureResult gfuture (SCM_LIST1 'cancelled) false)))))

 (define-cfn pause-mml ()
   ::void
   (Grv_LockGlobal)
   (set! mml-paused? true)
   (Grv_UnlockGlobal))

 (define-cfn resume-mml ()
   ::void
   (Grv_LockGlobal)
   (set! mml-paused? false)
   (Grv_UnlockGlobal))

 (define-cfn paused-mml? ()
   ::bool
   (Grv_LockGlobal)
   (let* ((paused?::bool mml-paused?))
     (Grv_UnlockGlobal)
     (return paused?)))

 (define-cfn compute-total-length (gsoundlet::GrvSoundlet*)
   ::int
   (let* ((cursor::GrvSoundlet* gsoundlet)
          (len::int 0))
     (while cursor
       (set! len (+ len (-> cursor length))
             cursor (-> cursor next)))
     (return len)))
 ) ;; end of inline-stub

(include "types.scm")

(define-cproc make-soundlet (type
                             freqs::<f64vector>
                             amps::<f64vector>
                             left-volume::<double>
                             right-volume::<double>
                             sec::<double>
                             attack-time::<double>
                             decay-time::<double>
                             sustain-level::<double>
                             release-time::<double>)
  ::<graviton-soundlet>
  (unless (== (SCM_F64VECTOR_SIZE freqs) (SCM_F64VECTOR_SIZE amps))
    (Scm_Error "freqs and amps must be the same size"))
  (let* ((length::int (cast int (round (* 44100 sec))))
         (gtone::GrvToneSoundlet* (SCM_NEW GrvToneSoundlet))
         (gsoundlet::GrvSoundlet* (SCM_NEW GrvSoundlet))
         (tone-type::GrvToneType))
    (cond
      ((SCM_EQ type 'silent)
       (set! tone-type TONE_SILENT))
      ((or (SCM_EQ type 'sine)
           (SCM_EQ type 'sin))
       (set! tone-type TONE_SINE))
      ((or (SCM_EQ type 'square)
           (SCM_EQ type 'square50))
       (set! tone-type TONE_SQUARE50))
      ((or (SCM_EQ type 'square12)
           (SCM_EQ type 'square125))
       (set! tone-type TONE_SQUARE12))
      ((SCM_EQ type 'square25)
       (set! tone-type TONE_SQUARE25))
      ((SCM_EQ type 'triangle)
       (set! tone-type TONE_TRIANGLE))
      ((SCM_EQ type 'sawtooth)
       (set! tone-type TONE_SAWTOOTH))
      ((or (SCM_EQ type 'noise)
           (SCM_EQ type 'long-noise))
       (set! tone-type TONE_LONG_NOISE))
      ((SCM_EQ type 'short-noise)
       (set! tone-type TONE_SHORT_NOISE))
      (else
       (Scm_Error "Invalid tone type: %S" type)))

    (set! (-> gtone type) tone-type
          (-> gtone freqs) (SCM_NEW_ATOMIC_ARRAY (.type double) (SCM_F64VECTOR_SIZE freqs))
          (-> gtone amps) (SCM_NEW_ATOMIC_ARRAY (.type double) (SCM_F64VECTOR_SIZE amps))
          (-> gtone num-freqs) (SCM_F64VECTOR_SIZE freqs)
          (-> gtone left-volume) left-volume
          (-> gtone right-volume) right-volume
          (-> gtone attack-time) (cast int (round (* 44100 attack-time)))
          (-> gtone decay-time) (cast int (round (* 44100 decay-time)))
          (-> gtone sustain-level) sustain-level
          (-> gtone release-time) (cast int (round (* 44100 release-time))))
    (memcpy (-> gtone freqs) (SCM_F64VECTOR_ELEMENTS freqs) (* (sizeof (.type double)) (SCM_F64VECTOR_SIZE freqs)))
    (memcpy (-> gtone amps) (SCM_F64VECTOR_ELEMENTS amps) (* (sizeof (.type double)) (SCM_F64VECTOR_SIZE amps)))
    (set! (-> gsoundlet next) NULL
          (-> gsoundlet type) SOUNDLET_TONE
          (-> gsoundlet length) length
          (ref (-> gsoundlet data) tone) gtone)
    (return gsoundlet)))

(define-cproc compose-soundlets (soundlets::<list>)
  ::<graviton-soundlet>
  (let* ((gcomposite::GrvCompositeSoundlet* (SCM_NEW GrvCompositeSoundlet))
         (gsoundlet::GrvSoundlet* (SCM_NEW GrvSoundlet)))
    (set! (-> gcomposite num-children) (Scm_Length soundlets)
          (-> gcomposite children) (SCM_NEW_ARRAY (.type GrvSoundlet*) (-> gcomposite num-children)))
    (let* ((i::int 0)
           (max-len::int 0))
      (for-each (lambda (child)
                  (unless (GRV_SOUNDLETP child)
                    (Scm_Error "<graviton-soundlet> required, but got %S" child))
                  (let* ((child-gsoundlet::GrvSoundlet* (GRV_SOUNDLET_PTR child))
                         (len::int 0))
                    (set! (aref (-> gcomposite children) i) child-gsoundlet
                          len (compute-total-length child-gsoundlet))
                    (when (< max-len len)
                      (set! max-len len)))
                  (pre++ i))
                soundlets)
      (set! (-> gsoundlet next) NULL
            (-> gsoundlet length) max-len
            (-> gsoundlet type) SOUNDLET_COMPOSITE
            (ref (-> gsoundlet data) composite) gcomposite))
    (return gsoundlet)))

(define-cproc append-soundlet! (gsoundlet1::<graviton-soundlet> gsoundlet2::<graviton-soundlet>)
  ::<void>
  (set! (-> gsoundlet1 next) gsoundlet2))

(define-cproc play-soundlet (gsoundlet::<graviton-soundlet>)
  (Mix_HaltMusic)

  (let* ((gcontext::GrvMMLMusicContext* (SCM_NEW (.type GrvMMLMusicContext)))
         (future (Grv_MakeFuture)))
    (set! (-> gcontext position) 0
          (-> gcontext num-soundlet-contexts) 16
          (-> gcontext soundlet-contexts) (SCM_NEW_ARRAY (.type GrvSoundletContext*) (-> gcontext num-soundlet-contexts))
          (-> gcontext future) future)
    (dotimes (i (-> gcontext num-soundlet-contexts))
      (set! (aref (-> gcontext soundlet-contexts) i) NULL))
    (retain-soundlet! gcontext gsoundlet 0)
    (cond
      ((Grv_IsPlayingMML)
       (enqueue-mml-music-context! gcontext))
      (else
       (Mix_HookMusic fill-audio-stream gcontext)))
    (return future)))

(define-cproc soundlet->wave-data (gsoundlet::<graviton-soundlet>)
  (let* ((len::int (compute-total-length gsoundlet))
         (buf (Scm_MakeS16Vector (* len 2) 0))
         (gcontext::GrvMMLMusicContext* (SCM_NEW (.type GrvMMLMusicContext)))
         (i::int))
    (set! (-> gcontext position) 0
          (-> gcontext num-soundlet-contexts) 16
          (-> gcontext soundlet-contexts) (SCM_NEW_ARRAY (.type GrvSoundletContext*) (-> gcontext num-soundlet-contexts))
          (-> gcontext future) SCM_FALSE)
    (dotimes (i (-> gcontext num-soundlet-contexts))
      (set! (aref (-> gcontext soundlet-contexts) i) NULL))
    (retain-soundlet! gcontext gsoundlet 0)
    (fill-audio-stream gcontext (cast Uint8* (SCM_S16VECTOR_ELEMENTS buf)) (* len 2 (sizeof int16_t)))
    (return buf)))



;;;
;;; Music
;;;

(inline-stub
 (define-cfn finalize-music (z data::void*)
   ::void
   (when (GRV_MUSICP z)
     (let* ((gmusic::GrvMusic* (GRV_MUSIC_PTR z)))
       (Mix_FreeMusic (-> gmusic music))
       (set! (-> gmusic music) NULL))))

 (define-cfn set-playing-music-context! (music-context::GrvMusicContext*)
   ::GrvMusicContext*
   (Grv_LockGlobal)
   (let* ((prev::GrvMusicContext* playing-music-context))
     (set! playing-music-context music-context)
     (Grv_UnlockGlobal)
     (return prev)))

 (define-cfn finish-music ()
   ::void :static
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (Grv_SetFutureResult gfuture (SCM_LIST1 'finished) false)
         (set! Grv_MusicLastFinishedTick (SDL_GetTicks))))))

 (define-cfn stop-music ()
   ::void
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (Grv_SetFutureResult gfuture (SCM_LIST1 'stopped) false)
         (set! Grv_MusicLastFinishedTick (SDL_GetTicks)))))
   (Mix_HaltMusic))
 ) ;; end of inline-stub

(define-cproc load-music (filename::<const-cstring>)
  (let* ((mmusic::Mix_Music* (Mix_LoadMUS filename)))
    (unless mmusic
      (Scm_Error "Mix_LoadMUS failed: %s" (Mix_GetError)))
    (let* ((gmusic::GrvMusic* (SCM_NEW (.type GrvMusic)))
           (music (GRV_MUSIC_BOX gmusic)))
      (set! (-> gmusic music) mmusic)
      (Scm_RegisterFinalizer music finalize-music NULL)
      (return music))))

(define-cproc play-music (music)
  (unless (GRV_MUSICP music)
    (Scm_Error "<graviton-music> required, but got %S" music))

  (Mix_HookMusicFinished finish-music)

  (stop-mml)
  (stop-music)

  (let* ((music-context::GrvMusicContext* (SCM_NEW (.type GrvMusicContext))))
    (set! (-> music-context music) music
          (-> music-context future) (Grv_MakeFuture))
    (set-playing-music-context! music-context)
    (when (< (Mix_PlayMusic (-> (GRV_MUSIC_PTR music) music) 0) 0)
      (set-playing-music-context! NULL)
      (Scm_Error "Mix_PlayMusic failed: %s" (Mix_GetError)))
    (return (-> music-context future))))

(define-cproc stop-music ()
  ::<void>
  (cond
    ((Grv_IsPlayingMML)
     (stop-mml))
    (else
     (stop-music))))

(define-cproc pause-music ()
  ::<void>
  (cond
    ((Grv_IsPlayingMML)
     (pause-mml))
    (else
     (Mix_PauseMusic))))

(define-cproc resume-music ()
  ::<void>
  (cond
    ((Grv_IsPlayingMML)
     (resume-mml))
    (else
     (Mix_ResumeMusic))))

(define-cproc playing-music? ()
  ::<boolean>
  (cond
    ((Grv_IsPlayingMML)
     (return true))
    (else
     (return (Mix_PlayingMusic)))))

(define-cproc paused-music? ()
  ::<boolean>
  (cond
    ((Grv_IsPlayingMML)
     (return (paused-mml?)))
    (else
     (return (Mix_PausedMusic)))))

(define-cproc set-music-volume! (volume::<int>)
  ::<void>
  (Mix_VolumeMusic volume))

(define-cproc music-volume ()
  ::<int>
  (return (Mix_VolumeMusic -1)))

