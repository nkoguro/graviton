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
extern void Grv_DecomposeRGBA(Uint32 color, Uint8 *r, Uint8 *g, Uint8 *b, Uint8 *a);


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
void Grv_SetFutureResult(GrvFuture *gfuture, ScmObj result, bool report_error);

/*
 * Image & Texture
 */

typedef struct GrvTransformParamRec {
  double m00;
  double m01;
  double m10;
  double m11;
  double x0;
  double y0;
  double left;
  double top;
  double right;
  double bottom;
} GrvTransformParam;

typedef struct GrvTextureRec {
  SDL_Texture *texture;
  int ref_count;
} GrvTexture;

extern ScmClass *GrvTextureClass;
#define GRV_TEXTURE_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvTexture*, obj)
#define GRV_TEXTUREP(obj) SCM_XTYPEP(obj, GrvTextureClass)
#define GRV_TEXTURE_BOX(ptr) Scm_MakeForeignPointer(GrvTextureClass, ptr)


typedef struct GrvImageRec {
  SDL_Surface *surface;
  SDL_Rect update_rect;
  GrvTransformParam param;
  ScmObj texture_alist;
} GrvImage;

extern ScmClass *GrvImageClass;
#define GRV_IMAGE_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvImage*, obj)
#define GRV_IMAGEP(obj) SCM_XTYPEP(obj, GrvImageClass)
#define GRV_IMAGE_BOX(ptr) Scm_MakeForeignPointer(GrvImageClass, ptr)


typedef struct GrvTileImageRec {
  ScmObj image;
  SDL_Rect rect;
} GrvTileImage;

extern ScmClass *GrvTileImageClass;
#define GRV_TILE_IMAGE_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvTileImage*, obj)
#define GRV_TILE_IMAGEP(obj) SCM_XTYPEP(obj, GrvTileImageClass)
#define GRV_TILE_IMAGE_BOX(ptr) Scm_MakeForeignPointer(GrvTileImageClass, ptr)


extern void Grv_RetainTexture(ScmObj win, GrvImage *gimage);
extern void Grv_ReleaseTexture(ScmObj win, GrvImage *gimage);
extern void Grv_RefreshTextures(GrvImage *gimage);
extern SDL_Texture *Grv_RetrieveTexture(ScmObj win, GrvImage *gimage);

extern void Grv_Bitblt(GrvImage *src_gimage, SDL_Rect *src_rect, GrvImage *dst_gimage, SDL_Rect *dst_rect, Uint32 color);
extern void Grv_SetNeedsRefreshImage(GrvImage *gimage, int x, int y, int w, int h);
extern void Grv_ComputeImageCoordinate(GrvImage *gimage, double x, double y, int *ox, int *oy);


/*
 * Window
 */

typedef struct GrvWindowRec {
  SDL_Window *window;
  SDL_Renderer *renderer;
  ScmObj sprites;
  ScmObj icon;
  int logical_width;
  int logical_height;
  int offset_x;
  int offset_y;
  ScmObj handler_table;
  SDL_Rect *clip;
} GrvWindow;

extern ScmClass *GrvWindowClass;
#define GRV_WINDOW_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvWindow*, obj)
#define GRV_WINDOWP(obj) SCM_XTYPEP(obj, GrvWindowClass)
#define GRV_WINDOW_BOX(ptr) Scm_MakeForeignPointer(GrvWindowClass, ptr)

extern ScmObj Grv_Windows;
extern int Grv_FramePerSecond;
extern ScmObj Grv_GlobalHandlerTable;
extern void Grv_DestroyWindow(GrvWindow *gwin);
extern void Grv_UpdateWindowContents();

/*
 * Sprite
 */

typedef struct GrvSpriteRec {
  ScmObj window;
  ScmObj image;
  SDL_Rect *srcrect;
  double center_x;
  double center_y;
  double z;
  double angle;
  double zoom_x;
  double zoom_y;
  bool visible;
  Uint32 color;
  SDL_Rect *clip;
} GrvSprite;

extern ScmClass *GrvSpriteClass;
#define GRV_SPRITE_PTR(obj) SCM_FOREIGN_POINTER_REF(GrvSprite*, obj)
#define GRV_SPRITEP(obj) SCM_XTYPEP(obj, GrvSpriteClass)
#define GRV_SPRITE_BOX(ptr) Scm_MakeForeignPointer(GrvSpriteClass, ptr)

extern void Grv_InvalidateSprite(GrvSprite *gsprite);
extern void Grv_RenderSprite(GrvSprite *gsprite);

#endif /* GRAVITON_H */