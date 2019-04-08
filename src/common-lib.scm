;;;
;;; common-lib.scm - Common utilities
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

(select-module graviton.common)

(inline-stub
 (declcode
  (.include "SDL.h"
            "gauche.h"
            "graviton.h"
            "string.h"))

 (define-cvar Grv_CustomEventType::Uint32)

 (define-cvar global-lock::SDL_SpinLock :static)
 (define-cvar object-pool::ScmObj* :static)
 (define-cvar object-pool-size::int :static)

 (.define OBJECT_POOL_INITIAL_SIZE 256)

 (initcode
  (set! Grv_CustomEventType (SDL_RegisterEvents 1))
  (when (== Grv_CustomEventType #xffffffff)
    (Scm_Error "SDL_RegisterEvents failed: %s" (SDL_GetError)))

  (set! global-lock 0)

  (set! object-pool-size OBJECT_POOL_INITIAL_SIZE
        object-pool (SCM_NEW_ARRAY (.type ScmObj) object-pool-size))
  (memset object-pool 0 (* (sizeof ScmObj) object-pool-size)))

 (define-cfn Grv_LockGlobal ()
   ::void
   (SDL_AtomicLock (& global-lock)))

 (define-cfn Grv_UnlockGlobal ()
   ::void
   (SDL_AtomicUnlock (& global-lock)))

 (define-cfn Grv_DecomposeRGBA (color::Uint32 r::Uint8* g::Uint8* b::Uint8* a::Uint8*)
   ::void
   (cond
     ((== SDL_BYTEORDER SDL_LIL_ENDIAN)
      (set! (* r) (logand color #xff)
            (* g) (logand (>> color 8) #xff)
            (* b) (logand (>> color 16) #xff)
            (* a) (logand (>> color 24) #xff)))
     (else
      (set! (* r) (logand (>> color 24) #xff)
            (* g) (logand (>> color 16) #xff)
            (* b) (logand (>> color 8) #xff)
            (* a) (logand color #xff)))))

 (define-cfn Grv_RetainObject (obj::ScmObj)
   ::void
   (when (or (== obj NULL)
             (not (SCM_PTRP obj)))
     (return))

   (let* ((i::int))
     (for ((set! i 0) (< i object-pool-size) (inc! i))
       (when (== (aref object-pool i) NULL)
         (set! (aref object-pool i) obj)
         (return)))
     (let* ((new-object-pool-size::int (* object-pool-size 2))
            (new-object-pool::ScmObj* (SCM_NEW_ARRAY (.type ScmObj) new-object-pool-size)))
       (memset new-object-pool 0 (* (sizeof ScmObj) new-object-pool-size))
       (memmove new-object-pool object-pool object-pool-size)
       (set! object-pool new-object-pool
             object-pool-size new-object-pool-size)
       (Grv_RetainObject obj))))

 (define-cfn Grv_ReleaseObject (obj::ScmObj)
   ::void
   (when (or (== obj NULL)
             (not (SCM_PTRP obj)))
     (return))

   (let* ((i::int))
     (for ((set! i 0) (< i object-pool-size) (inc! i))
       (when (== (aref object-pool i) obj)
         (set! (aref object-pool i) NULL)
         (return)))))

 ) ;; end of inline-stub


