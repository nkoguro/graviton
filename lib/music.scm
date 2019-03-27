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


;;;
;;; MML
;;;

(inline-stub
 (define-cfn playing-mml? ()
   ::bool :static
   (return (!= (Mix_GetMusicHookData) NULL)))

 (define-cfn enqueue-mml-music-context! (gcontext::GrvMMLMusicContext*)
   ::void :static
   (lock-global-var)
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
   (unlock-global-var))

 (define-cfn dequeue-mml-music-context! ()
   ::GrvMMLMusicContext* :static
   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (lock-global-var)
     (unless (== (ref mml-music-context-queue start) (ref mml-music-context-queue end))
       (set! gcontext (aref (ref mml-music-context-queue buf) (ref mml-music-context-queue start))
             (ref mml-music-context-queue start) (% (+ (ref mml-music-context-queue start) 1)
                                                    (ref mml-music-context-queue length))))
     (unlock-global-var)
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
                (GRV_FUTURE_P (-> gcontext future)))
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
                  (GRV_FUTURE_P (-> gcontext future)))
         (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
           (set-future-result! gfuture (SCM_LIST1 'finished) false))

         (let* ((next-gcontext::GrvMMLMusicContext* (dequeue-mml-music-context!)))
           (cond
             (next-gcontext
              (set! (-> gcontext position) (-> next-gcontext position)
                    (-> gcontext soundlet-contexts) (-> next-gcontext soundlet-contexts)
                    (-> gcontext num-soundlet-contexts) (-> next-gcontext num-soundlet-contexts)
                    (-> gcontext future) (-> next-gcontext future)))
             (else
              (let* ((event::SDL_Event))
                (set! (ref event type) graviton-event-type
                      (ref event user code) GRAVITON_MML_FINISH_CODE)
                (SDL_PushEvent (& event))))))))))

 (define-cfn stop-mml ()
   ::void
   (let* ((gcontext::GrvMMLMusicContext* (Mix_GetMusicHookData)))
     (when gcontext
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (set-future-result! gfuture (SCM_LIST1 'stopped) false))))
   (Mix_HookMusic NULL NULL)

   (let* ((gcontext::GrvMMLMusicContext* NULL))
     (while (= gcontext (dequeue-mml-music-context!))
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> gcontext future))))
         (set-future-result! gfuture (SCM_LIST1 'cancelled) false)))))

 (define-cfn pause-mml ()
   ::void
   (lock-global-var)
   (set! mml-paused? true)
   (unlock-global-var))

 (define-cfn resume-mml ()
   ::void
   (lock-global-var)
   (set! mml-paused? false)
   (unlock-global-var))

 (define-cfn paused-mml? ()
   ::bool
   (lock-global-var)
   (let* ((paused?::bool mml-paused?))
     (unlock-global-var)
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
         (tone-type::ToneType))
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
                  (unless (GRV_SOUNDLET_P child)
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
         (future (make-future)))
    (set! (-> gcontext position) 0
          (-> gcontext num-soundlet-contexts) 16
          (-> gcontext soundlet-contexts) (SCM_NEW_ARRAY (.type GrvSoundletContext*) (-> gcontext num-soundlet-contexts))
          (-> gcontext future) future)
    (dotimes (i (-> gcontext num-soundlet-contexts))
      (set! (aref (-> gcontext soundlet-contexts) i) NULL))
    (retain-soundlet! gcontext gsoundlet 0)
    (cond
      ((playing-mml?)
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

(inline-stub
 (define-cfn finalize-music (z data::void*)
   ::void
   (when (GRV_MUSIC_P z)
     (let* ((gmusic::GrvMusic* (GRV_MUSIC_PTR z)))
       (Mix_FreeMusic (-> gmusic music))
       (set! (-> gmusic music) NULL))))

 (define-cfn set-playing-music-context! (music-context::GrvMusicContext*)
   ::GrvMusicContext*
   (lock-global-var)
   (let* ((prev::GrvMusicContext* playing-music-context))
     (set! playing-music-context music-context)
     (unlock-global-var)
     (return prev)))

 (define-cfn finish-music ()
   ::void :static
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (set-future-result! gfuture (SCM_LIST1 'finished) false)
         (set! music-last-finished-tick (SDL_GetTicks))))))

 (define-cfn stop-music ()
   ::void
   (let* ((music-context::GrvMusicContext* (set-playing-music-context! NULL)))
     (when music-context
       (let* ((gfuture::GrvFuture* (GRV_FUTURE_PTR (-> music-context future))))
         (set-future-result! gfuture (SCM_LIST1 'stopped) false)
         (set! music-last-finished-tick (SDL_GetTicks)))))
   (Mix_HaltMusic))
 ) ;; end of inline-stub

(define-cproc load-music (filename::<const-cstring>)
  (let* ((mmusic::Mix_Music* (Mix_LoadMUS filename)))
    (unless mmusic
      (Scm_Error "Mix_LoadMUS failed: %s" (Mix_GetError)))
    (let* ((gmusic::GrvMusic* (SCM_NEW (.type GrvMusic)))
           (music (MAKE_GRV_MUSIC gmusic)))
      (set! (-> gmusic music) mmusic)
      (Scm_RegisterFinalizer music finalize-music NULL)
      (return music))))

(define-cproc play-music (music)
  (unless (GRV_MUSIC_P music)
    (Scm_Error "<graviton-music> required, but got %S" music))

  (stop-mml)
  (stop-music)

  (let* ((music-context::GrvMusicContext* (SCM_NEW (.type GrvMusicContext))))
    (set! (-> music-context music) music
          (-> music-context future) (make-future))
    (set-playing-music-context! music-context)
    (when (< (Mix_PlayMusic (-> (GRV_MUSIC_PTR music) music) 0) 0)
      (set-playing-music-context! NULL)
      (Scm_Error "Mix_PlayMusic failed: %s" (Mix_GetError)))
    (return (-> music-context future))))

(define-cproc stop-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (stop-mml))
    (else
     (stop-music))))

(define-cproc pause-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (pause-mml))
    (else
     (Mix_PauseMusic))))

(define-cproc resume-music ()
  ::<void>
  (cond
    ((playing-mml?)
     (resume-mml))
    (else
     (Mix_ResumeMusic))))

(define-cproc playing-music? ()
  ::<boolean>
  (cond
    ((playing-mml?)
     (return true))
    (else
     (return (Mix_PlayingMusic)))))

(define-cproc paused-music? ()
  ::<boolean>
  (cond
    ((playing-mml?)
     (return (paused-mml?)))
    (else
     (return (Mix_PausedMusic)))))

(define-cproc set-music-volume! (volume::<int>)
  ::<void>
  (Mix_VolumeMusic volume))

(define-cproc music-volume ()
  ::<int>
  (return (Mix_VolumeMusic -1)))

(set! (setter music-volume) set-music-volume!)
