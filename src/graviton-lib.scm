;;;
;;; graviton-lib.scm - Graphics and sound module
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

(select-module graviton)

(inline-stub
 (declcode
  (.include "SDL.h"
            "SDL_image.h"
            "SDL_mixer.h"
            "gauche.h"
            "graviton.h")
  ) ;; end of declcode

 (define-cfn teardown-libs (data::|void*|)
   ::void
   (Mix_CloseAudio)
   (Mix_Quit)
   (SDL_Quit))

 (define-cfn initialize-libs ()
   ::void
   (SDL_Init (logior SDL_INIT_VIDEO SDL_INIT_AUDIO))
   (Mix_Init (logior MIX_INIT_FLAC MIX_INIT_MOD MIX_INIT_MP3 MIX_INIT_OGG))
   (when (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 2048)
     (Scm_Error "Mix_OpenAudio failed: %s" (Mix_GetError)))
   (IMG_Init (logior IMG_INIT_JPG IMG_INIT_PNG IMG_INIT_TIF))
   (Mix_AllocateChannels GRV_CHANNEL_SIZE)

   (Scm_AddCleanupHandler teardown-libs NULL))

 (initcode
  (initialize-libs))
 ) ;; end of inline-stub
