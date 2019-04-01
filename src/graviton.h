/**
 *  graviton.h - Graphics and sound module
 *
 *   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef GRAVITON_H
#define GRAVITON_H

#include <stdbool.h>
#include "SDL.h"
#include "gauche.h"

/*
 * Common utilities
 */
extern Uint32 Grv_CustomEventType;

extern void Grv_LockGlobal();
extern void Grv_UnlockGlobal();

/*
 * Custom Event
 */
#define GRV_EVENT_EXCEPTION 1
#define GRV_EVENT_MML_FINISH 2
#define GRV_EVENT_APPLY 3
#define GRV_EVENT_WINDOW_UPDATE 4

#define GRV_SEND_EVENT(event_code, arg0, arg1) \
  do { \
    SDL_Event event; \
    event.type = Grv_CustomEventType; \
    event.user.code = event_code; \
    event.user.data1 = arg0; \
    event.user.data2 = arg1; \
    SDL_PushEvent(&event); \
  } while (0);

#define GRV_APPLY(proc, args) GRV_SEND_EVENT(GRV_EVENT_APPLY, proc, args)
#define GRV_NOTIFY_STACKTRACE(stacktrace) GRV_SEND_EVENT(GRV_EVENT_EXCEPTION, stacktrace, NULL)

/*
 * Async
 */

typedef struct GrvFutureRec {
  SDL_mutex *lock;
  SDL_cond *cond;
  ScmObj result;
  ScmObj exception;
  char *message;
  ScmObj continuations;
  bool consumed;
} GrvFuture;

extern ScmClass *GrvFutureClass;
#define GRV_FUTURE_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvFuture*, obj)
#define GRV_FUTUREP(obj) SCM_XTYPEP(obj, GrvFutureClass)
#define GRV_FUTURE_BOX(ptr) Scm_MakeForeignPointer(GrvFutureClass, ptr)

ScmObj Grv_MakeFuture();
void Grv_SetFutureResult(GrvFuture* gfuture, ScmObj result, bool report_error);

#endif /* GRAVITON_H */