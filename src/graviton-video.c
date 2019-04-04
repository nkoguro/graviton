/**
 *  graviton-video.c - Video
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

#include <gauche.h>
#include <gauche/extend.h>
#include "graviton.h"

extern void Scm_Init_image_lib();
extern void Scm_Init_window_lib();
extern void Scm_Init_sprite_lib();
extern void Scm_Init_tilemap_lib();

ScmObj Grv_GravitonVideoModule;

ScmClass *GrvTextureClass;
ScmClass *GrvImageClass;
ScmClass *GrvTileImageClass;
ScmClass *GrvWindowClass;
ScmClass *GrvSpriteClass;
ScmClass *GrvTileMapClass;

void Scm_Init_graviton_video(void)
{
  ScmModule *mod;
  SCM_INIT_EXTENSION(graviton_video);
  mod = SCM_MODULE(SCM_FIND_MODULE("graviton.video", TRUE));
  Grv_GravitonVideoModule = SCM_OBJ(mod);
  GrvTextureClass = Scm_MakeForeignPointerClass(mod, "<graviton-texture>", NULL, NULL, 0);
  GrvImageClass = Scm_MakeForeignPointerClass(mod, "<graviton-image>", NULL, NULL, 0);
  GrvTileImageClass = Scm_MakeForeignPointerClass(mod, "<graviton-tile-image>", NULL, NULL, 0);
  GrvWindowClass = Scm_MakeForeignPointerClass(mod, "<graviton-window>", NULL, NULL, 0);
  GrvSpriteClass = Scm_MakeForeignPointerClass(mod, "<graviton-sprite>", NULL, NULL, 0);
  GrvTileMapClass = Scm_MakeForeignPointerClass(mod, "<graviton-tile-map>", NULL, NULL, 0);
  Scm_Init_image_lib();
  Scm_Init_window_lib();
  Scm_Init_sprite_lib();
  Scm_Init_tilemap_lib();
}

