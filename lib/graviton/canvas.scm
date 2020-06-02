;;;
;;; canvas.scm - Graviton canvas library
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

(define-module graviton.canvas
  (use file.util)
  (use gauche.parameter)
  (use graviton)
  (use graviton.config)
  (use graviton.jsise)
  (use util.list)

  (export make-canvas
          load-canvas
          current-canvas
          set-canvas-visible!

          linear-gradient
          radial-gradient
          pattern
          current-fill-style
          set-fill-style!
          current-font
          set-font!
          current-global-alpha
          set-global-alpha!
          current-global-composite-operation
          set-global-composite-operation!
          current-image-smoothing-enabled?
          set-image-smoothing-enabled!
          current-line-cap
          set-line-cap!
          current-line-dash
          set-line-dash!
          current-line-dash-offset
          set-line-dash-offset!
          current-line-join
          set-line-join!
          current-line-width
          set-line-width!
          current-miter-limit
          set-miter-limit!
          current-shadow-blur
          set-shadow-blur!
          current-shadow-color
          set-shadow-color!
          current-shadow-offset-x
          set-shadow-offset-x!
          current-shadow-offset-y
          set-shadow-offset-y!
          current-stroke-style
          set-stroke-style!
          current-text-align
          set-text-align!
          current-text-baseline
          set-text-baseline!

          arc
          arc-to
          begin-path
          bezier-curve-to
          clear-rect
          clip
          close-path
          draw-canvas
          ellipse
          fill
          fill-rect
          fill-text
          get-image-data
          is-point-in-path?
          is-point-in-stroke?
          line-to
          measure-text
          move-to
          put-image-data
          quadratic-curve-to
          rect
          restore-context
          rotate
          save-context
          scale
          set-transform!
          stroke
          stroke-rect
          stroke-text
          transform
          translate
          create-image-data
          upload-image-data
          download-image-data))

(select-module graviton.canvas)

(import-js ("graviton/canvas.mjs" :as Canvas))

(define current-canvas (make-parameter #f))

(define-class <canvas> (<proxy-object>)
  ((width :init-keyword :width)
   (height :init-keyword :height)
   (z :init-keyword :z)
   (visible? :init-keyword :visible?)
   (context2d-list :init-form (list (make <context2d>)))
   (context2d :allocation :virtual
              :slot-ref (lambda (canvas)
                          (car (slot-ref canvas 'context2d-list))))))

(define-class <context2d> ()
  ((fill-style :init-value "#000000")
   (font :init-value "10px sans-serif")
   (global-alpha :init-value 1.0)
   (global-composite-operation :init-value 'source-over)
   (image-smoothing-enabled :init-value #t)
   (line-cap :init-value 'butt)
   (line-dash :init-value #())
   (line-dash-offset :init-value 0)
   (line-join :init-value 'miter)
   (line-width :init-value 1.0)
   (miter-limit :init-value 10.0)
   (shadow-blur :init-value 0)
   (shadow-color :init-value "rgba(0, 0, 0, 0)")
   (shadow-offset-x :init-value 0)
   (shadow-offset-y :init-value 0)
   (stroke-style :init-value "#000000")
   (text-align :init-value 'start)
   (text-baseline :init-value 'alphabetic)))

(define (copy-context2d ctx)
  (let1 ctx2 (make <context2d>)
    (for-each (lambda (slot)
                (let1 slot-name (car slot)
                  (slot-set! ctx2 slot-name (slot-ref ctx slot-name))))
              (class-direct-slots <context2d>))
    ctx2))

(define-class <graviton-image> (<proxy-object>)
  ((width :init-keyword :width)
   (height :init-keyword :height)))

(define-class <linear-gradient> ()
  ((x0 :init-keyword :x0)
   (y0 :init-keyword :y0)
   (x1 :init-keyword :x1)
   (y1 :init-keyword :y1)
   (color-stops :init-keyword :color-stops)))

(define (linear-gradient x0 y0 x1 y1 color-stops)
  (make <linear-gradient> :x0 x0 :y0 y0 :x1 x1 :y1 y1 :color-stops color-stops))

(define-method style->json ((linear-gradient <linear-gradient>))
  `(("type" . "linear-gradient")
    ("x0" . ,(slot-ref linear-gradient 'x0))
    ("y0" . ,(slot-ref linear-gradient 'y0))
    ("x1" . ,(slot-ref linear-gradient 'x1))
    ("y1" . ,(slot-ref linear-gradient 'y1))
    ("color-stops" . ,(list->vector (map list->vector (slot-ref linear-gradient 'color-stops))))))

(define-class <radial-gradient> ()
  ((x0 :init-keyword :x0)
   (y0 :init-keyword :y0)
   (r0 :init-keyword :r0)
   (x1 :init-keyword :x1)
   (y1 :init-keyword :y1)
   (r1 :init-keyword :r1)
   (color-stops :init-keyword :color-stops)))

(define (radial-gradient x0 y0 r0 x1 y1 r1 color-stops)
  (make <radial-gradient> :x0 x0 :y0 y0 :r0 r0 :x1 x1 :y1 y1 :r1 r1 :color-stops color-stops))

(define-method style->json ((radial-gradient <radial-gradient>))
  `(("type" . "radial-gradient")
    ("x0" . ,(slot-ref radial-gradient 'x0))
    ("y0" . ,(slot-ref radial-gradient 'y0))
    ("r0" . ,(slot-ref radial-gradient 'r0))
    ("x1" . ,(slot-ref radial-gradient 'x1))
    ("y1" . ,(slot-ref radial-gradient 'y1))
    ("r1" . ,(slot-ref radial-gradient 'r1))
    ("color-stops" . ,(list->vector (map list->vector (slot-ref radial-gradient 'color-stops))))))

(define-method style->json ((color <string>))
  color)

(define-class <pattern> ()
  ((canvas :init-keyword :canvas
           :init-value #f)
   (image :init-keyword :image
          :init-value #f)
   (repetition :init-keyword :repetition)))

(define-constant repetition-alist
  '((repeat . "repeat")
    (repeat-x . "repeat-x")
    (repeat-y . "repeat-y")
    (no-repeat . "no-repeat")))

(define (repetition-name repetition)
  (or (assoc-ref repetition-alist repetition #f)
      (errorf "Invalid repetition: ~s" repetition)))

(define-method pattern ((canvas <canvas>) :optional (repetition 'repeat))
  (make <pattern> :canvas canvas :repetition (repetition-name repetition)))

(define-method pattern ((image <graviton-image>) :optional (repetition 'repeat))
  (make <pattern> :image image :repetition (repetition-name repetition)))

(define-method style->json ((pattern <pattern>))
  `(("type" . "pattern")
    ("canvas" . ,(and-let1 canvas (slot-ref pattern 'canvas)
                   (proxy-id canvas)))
    ("image" . ,(and-let1 image (slot-ref pattern 'image)
                  (proxy-id image)))
    ("repetition" . ,(slot-ref pattern 'repetition))))


(define-jsvar *working-images* (make Set))

(define (make-canvas width height :key (z 0) (visible? #t))
  (let1 canvas (make <canvas> :width width :height height :z z :visible? visible?)
    (jslet ((canvas*::object* canvas)
            (width::u32)
            (height::u32)
            (z::u32)
            (visible?::boolean))
      (set! canvas*.value (Canvas.createCanvas width height z visible?)))
    (when visible?
      (current-canvas canvas))
    canvas))

(define (load-canvas filename :key (z 0) (visible? #t) (content-type #f))
  (let1 canvas (make <canvas> :z z :visible? visible?)
    (transform-future
        (jslet/result ((canvas*::object* canvas)
                       (url::string (resource-url filename :content-type content-type))
                       (z::u32)
                       (visible?::boolean))
          (let ((img (make Image)))
            (*working-images*.add img)
            (set! img.src url)
            (set! img.onload (lambda ()
                               (let* ((canvas (Canvas.createCanvas img.width img.height z visible?))
                                      (ctx (canvas.getContext "2d")))
                                 (set! canvas*.value canvas)
                                 (ctx.drawImage img 0 0)
                                 (*working-images*.delete img)
                                 (result canvas.width canvas.height))))
            (set! img.onerror (lambda ()
                                (*working-images*.delete img)
                                (raise "Load image failed.")))))
      (lambda (w h)
        (slot-set! canvas 'width w)
        (slot-set! canvas 'height h)
        canvas))))

(define (set-canvas-visible! canvas visible?)
  (jslet ((canvas::object)
          (visible?::boolean))
    (cond
      (visible?
       (set! canvas.style.visibility "visible")
       (when window.isElectron
         (window.showBrowserWindow)))
      (else
       (set! canvas.style.visibility "hidden"))))
  (slot-set! canvas 'visible? visible?))

(define (current-fill-style)
  (~ (current-canvas) 'context2d 'fill-style))

(define (set-fill-style! style)
  (set! (~ (current-canvas) 'context2d 'fill-style) style)
  (jslet ((canvas::object (current-canvas))
          (style-json::json (style->json style)))
    (let ((ctx (canvas.getContext "2d")))
      (set! ctx.fillStyle (Canvas.obj2style ctx style-json)))))

(define (current-font)
  (~ (current-canvas) 'context2d 'font))

(define (set-font! font)
  (set! (~ (current-canvas) 'context2d 'font) font)
  (jslet ((canvas::object (current-canvas))
          (font::string))
    (set! (ref (canvas.getContext "2d") 'font) font)))

(define (current-global-alpha)
  (~ (current-canvas) 'context2d 'global-alpha))

(define (set-global-alpha! alpha)
  (set! (~ (current-canvas) 'context2d 'global-alpha) alpha)
  (jslet ((canvas::object (current-canvas))
          (alpha::f64))
    (set! (ref (canvas.getContext "2d") 'globalAlpha) alpha)))

(define (current-global-composite-operation)
  (~ (current-canvas) 'context2d 'global-composite-operation))

(define-jsenum global-composite-operation-enum
  (source-over "source-over")
  (source-in "source-in")
  (source-out "source-out")
  (source-atop "source-atop")
  (destination-over "destination-over")
  (destination-in "destination-in")
  (destination-out "destination-out")
  (destination-atop "destination-atop")
  (lighter "lighter")
  (copy "copy")
  (xor "xor")
  (multiply "multiply")
  (screen "screen")
  (overlay "overlay")
  (darken "darken")
  (lighten "lighten")
  (color-dodge "color-dodge")
  (color-burn "color-burn")
  (hard-light "hard-light")
  (soft-light "soft-light")
  (difference "difference")
  (exclusion "exclusion")
  (hue "hue")
  (saturation "saturation")
  (color "color")
  (luminosity "luminosity"))

(define (set-global-composite-operation! op)
  (set! (~ (current-canvas) 'context2d 'global-composite-opration) op)
  (jslet ((canvas::object (current-canvas))
          (op::global-composite-operation-enum))
    (set! (ref (canvas.getContext "2d") 'globalCompositeOperation) op)))

(define (current-image-smoothing-enabled?)
  (~ (current-canvas) 'context2d 'image-smoothing-enabled))

(define (set-image-smoothing-enabled! flag)
  (set! (~ (current-canvas) 'context2d 'image-smoothing-enabled) flag)
  (jslet ((canvas::object (current-canvas))
          (flag::boolean))
    (set! (ref (canvas.getContext "2d") 'imageSmoothingEnabled) flag)))

(define (current-line-cap)
  (~ (current-canvas) 'context2d 'line-cap))

(define-jsenum line-cap-enum
  (butt "butt")
  (round "round")
  (square "square"))

(define (set-line-cap! opt)
  (set! (~ (current-canvas) 'context2d 'line-cap) opt)
  (jslet ((canvas::object (current-canvas))
          (opt::line-cap-enum))
    (set! (ref (canvas.getContext "2d") 'lineCap) opt)))

(define (current-line-dash)
  (~ (current-canvas) 'context2d 'line-dash))

(define (set-line-dash! segments)
  (set! (~ (current-canvas) 'context2d 'line-dash) segments)
  (jslet ((canvas::object (current-canvas))
          (segments::json))
    ((ref (canvas.getContext "2d") 'setLineDash) segments)))

(define (current-line-dash-offset)
  (~ (current-canvas) 'context2d 'line-dash-offset))

(define (set-line-dash-offset! offset)
  (set! (~ (current-canvas) 'context2d 'line-dash-offset) offset)
  (jslet ((canvas::object (current-canvas))
          (offset::f64))
    (set! (ref (canvas.getContext "2d") 'lineDashOffset) offset)))

(define (current-line-join)
  (~ (current-canvas) 'context2d 'line-join))

(define-jsenum line-join-enum
  (bevel "bevel")
  (round "round")
  (miter "miter"))

(define (set-line-join! opt)
  (set! (~ (current-canvas) 'context2d 'line-join) opt)
  (jslet ((canvas::object (current-canvas))
          (opt::line-join-enum))
    (set! (ref (canvas.getContext "2d") 'lineJoin) opt)))

(define (current-line-width)
  (~ (current-canvas) 'context2d 'line-width))

(define (set-line-width! w)
  (set! (~ (current-canvas) 'context2d 'line-width) w)
  (jslet ((canvas::object (current-canvas))
          (w::f64))
    (set! (ref (canvas.getContext "2d") 'lineWidth) w)))

(define (current-miter-limit)
  (~ (current-canvas) 'context2d 'miter-limit))

(define (set-miter-limit! limit)
  (set! (~ (current-canvas) 'context2d 'miter-limit) limit)
  (jslet ((canvas::object (current-canvas))
          (limit::f64))
    (set! (ref (canvas.getContext "2d") 'miterLimit) limit)))

(define (current-shadow-blur)
  (~ (current-canvas) 'context2d 'shadow-blur))

(define (set-shadow-blur! level)
  (set! (~ (current-canvas) 'context2d 'shadow-blur) level)
  (jslet ((canvas::object (current-canvas))
          (level::f64))
    (set! (ref (canvas.getContext "2d") 'shadowBlur) level)))

(define (current-shadow-color)
  (~ (current-canvas) 'context2d 'shadow-color))

(define (set-shadow-color! color)
  (set! (~ (current-canvas) 'context2d 'shadow-color) color)
  (jslet ((canvas::object (current-canvas))
          (color::string))
    (set! (ref (canvas.getContext "2d") 'shadowColor) color)))

(define (current-shadow-offset-x)
  (~ (current-canvas) 'context2d 'shadow-offset-x))

(define (set-shadow-offset-x! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-x) offset)
  (jslet ((canvas::object (current-canvas))
          (offset::f64))
    (set! (ref (canvas.getContext "2d") 'shadowOffsetX) offset)))

(define (current-shadow-offset-y)
  (~ (current-canvas) 'context2d 'shadow-offset-y))

(define (set-shadow-offset-y! offset)
  (set! (~ (current-canvas) 'context2d 'shadow-offset-y) offset)
  (jslet ((canvas::object (current-canvas))
          (offset::f64))
    (set! (ref (canvas.getContext "2d") 'shadowOffsetY) offset)))

(define (current-stroke-style)
  (~ (current-canvas) 'context2d 'stroke-style))

(define (set-stroke-style! style)
  (set! (~ (current-canvas) 'context2d 'stroke-style) style)
  (jslet ((canvas::object (current-canvas))
          (style-json::json (style->json style)))
    (let ((ctx (canvas.getContext "2d")))
      (set! ctx.strokeStyle (Canvas.obj2style ctx style-json)))))

(define (current-text-align)
  (~ (current-canvas) 'context2d 'text-align))

(define-jsenum text-align-enum
  (left "left")
  (right "right")
  (center "center")
  (start "start")
  (end "end"))

(define (set-text-align! align)
  (set! (~ (current-canvas) 'context2d 'text-align) align)
  (let ((canvas (current-canvas)))
    (set! (ref (canvas.getContext "2d") 'textAlign) align)))

(define (current-text-baseline)
  (~ (current-canvas) 'context2d 'text-baseline))

(define-jsenum text-baseline-enum
  (top "top")
  (hanging "hanging")
  (middle "middle")
  (alphabetic "alphabetic")
  (ideographic "ideographic")
  (bottom "bottom"))

(define (set-text-baseline! opt)
  (set! (~ (current-canvas) 'context2d 'text-baseline) opt)
  (let ((canvas (current-canvas)))
    (set! (ref (canvas.getContext "2d") 'textBaseline) opt)))

(define (arc x y radius start-angle end-angle :optional (anti-clockwise #f))
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (radius::s32)
          (start-angle::f64)
          (end-angle::f64)
          (anti-clockwise::boolean))
    ((ref (canvas.getContext "2d") 'arc) x y radius start-angle end-angle anti-clockwise)))

(define (arc-to x1 y1 x2 y2 radius)
  (jslet ((canvas::object (current-canvas))
          (x1::s32)
          (y1::s32)
          (x2::s32)
          (y2::s32)
          (radius::s32))
    ((ref (canvas.getContext "2d") 'arcTo) x1 y1 x2 y2 radius)))

(define (begin-path)
  (jslet ((canvas::object (current-canvas)))
    ((ref (canvas.getContext "2d") 'beginPath))))

(define (bezier-curve-to cp1x cp1y cp2x cp2y x y)
  (jslet ((canvas::object (current-canvas))
          (cp1x::s32)
          (cp1y::s32)
          (cp2x::s32)
          (cp2y::s32)
          (x::s32)
          (y::s32))
    ((ref (canvas.getContext "2d") 'bezierCurveTo) cp1x cp1y cp2x cp2y x y)))

(define (clear-rect x y w h)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (w::s32)
          (h::s32))
    ((ref (canvas.getContext "2d") 'clearRect) x y w h)))

(define-jsenum fillrule-enum
  (nonzero "nonzero")
  (evenodd "evenodd"))

(define (clip :optional (rule 'nonzero))
  (jslet ((canvas::object (current-canvas))
          (rule::fillrule-enum))
    ((ref (canvas.getContext "2d") 'clip) rule)))

(define (close-path)
  (jslet ((canvas::object (current-canvas)))
    ((ref (canvas.getContext "2d") 'closePath))))

(define-method draw-canvas ((src-canvas <canvas>) dx dy)
  (jslet ((canvas::object (current-canvas))
          (src-canvas::object)
          (dx::s32)
          (dy::s32))
    ((ref (canvas.getContext "2d") 'drawImage) src-canvas dx dy)))

(define-method draw-canvas ((src-canvas <canvas>) dx dy dw dh)
  (jslet ((canvas::object (current-canvas))
          (src-canvas::object)
          (dx::s32)
          (dy::s32)
          (dw::s32)
          (dh::s32))
    ((ref (canvas.getContext "2d") 'drawImage) src-canvas dx dy dw dh)))

(define-method draw-canvas ((src-canvas <canvas>) sx sy sw sh dx dy dw dh)
  (jslet ((canvas::object (current-canvas))
          (src-canvas::object)
          (sx::s32)
          (sy::s32)
          (sw::s32)
          (sh::s32)
          (dx::s32)
          (dy::s32)
          (dw::s32)
          (dh::s32))
    ((ref (canvas.getContext "2d") 'drawImage) src-canvas sx sy sw sh dx dy dw dh)))

(define (ellipse x y radius-x radius-y rotation start-angle end-angle :optional (anti-clockwise? #f))
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (radius-x::s32)
          (radius-y::s32)
          (rotation::f64)
          (start-angle::f64)
          (end-angle::f64)
          (anti-clockwise?::boolean))
    ((ref (canvas.getContext "2d") 'ellipse)
     x y radius-x radius-y rotation start-angle end-angle anti-clockwise?)))

(define (fill :optional (rule 'nonzero))
  (jslet ((canvas::object (current-canvas))
          (rule::fillrule-enum))
    ((ref (canvas.getContext "2d") 'fill) rule)))

(define (fill-rect x y w h)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (w::s32)
          (h::s32))
    ((ref (canvas.getContext "2d") 'fillRect) x y w h)))

(define (fill-text text x y :optional (max-width 0))
  (jslet ((canvas::object (current-canvas))
          (text::string)
          (x::s32)
          (y::s32)
          (max-width::s32))
    (if (< 0 max-width)
        ((ref (canvas.getContext "2d") 'fillText) text x y max-width)
        ((ref (canvas.getContext "2d") 'fillText) text x y))))

(define (get-image-data sx sy sw sh)
  (let1 image (make <graviton-image> :width sw :height sh)
    (jslet ((canvas::object (current-canvas))
            (image*::object* image)
            (sx::s32)
            (sy::s32)
            (sw::s32)
            (sh::s32))
      (let ((image ((ref (canvas.getContext "2d") 'getImageData) sx sy sw sh)))
        (set! image*.value image)))
    image))

(define (is-point-in-path? x y :optional (rule 'nonzero))
  (jslet/result ((canvas::object (current-canvas))
                 (x::s32)
                 (y::s32)
                 (rule::fillrule-enum))
    (result ((ref (canvas.getContext "2d") 'isPointInPath) x y rule))))

(define (is-point-in-stroke? x y)
  (jslet/result ((canvas::object (current-canvas))
                 (x::s32)
                 (y::s32))
    (result ((ref (canvas.getContext "2d") 'isPointInStroke) x y))))

(define (line-to x y)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32))
    ((ref (canvas.getContext "2d") 'lineTo) x y)))

(define-class <text-metrics> ()
  ((width :init-keyword :width)))

(define (measure-text text)
  (transform-future
      (jslet/result ((canvas::object (current-canvas))
                     (text::string))
        (let ((text-metrics ((ref (canvas.getContext "2d") 'measureText) text)))
          (result text-metrics.width)))
    (lambda (width)
      (make <text-metrics> :width width))))

(define (move-to x y)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32))
    ((ref (canvas.getContext "2d") 'moveTo) x y)))

(define-method put-image-data ((image <graviton-image>) dx dy)
  (jslet ((canvas::object (current-canvas))
          (image::object)
          (dx::s32)
          (dy::s32))
    ((ref (canvas.getContext "2d") 'putImageData) image dx dy)))

(define-method put-image-data ((image <graviton-image>) dx dy dirty-x dirty-y dirty-width dirty-height)
  (jslet ((canvas::object (current-canvas))
          (image::object)
          (dx::s32)
          (dy::s32)
          (dirty-x::s32)
          (dirty-y::s32)
          (dirty-width::s32)
          (dirty-height::s32))
    ((ref (canvas.getContext "2d") 'putImageData) image dx dy dirty-x dirty-y dirty-width dirty-height)))

(define (quadratic-curve-to cpx cpy x y)
  (jslet ((canvas::object (current-canvas))
          (cpx::s32)
          (cpy::s32)
          (x::s32)
          (y::s32))
    ((ref (canvas.getContext "2d") 'quadraticCurveTo) cpx cpy x y)))

(define (rect x y w h)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (w::s32)
          (h::s32))
    ((ref (canvas.getContext "2d") 'rect) x y w h)))

(define (restore-context)
  (jslet ((canvas::object (current-canvas)))
    ((ref (canvas.getContext "2d") 'restore)))
  (pop! (slot-ref (current-canvas) 'context2d-list)))

(define (rotate angle)
  (jslet ((canvas::object (current-canvas))
          (angle::f64))
    ((ref (canvas.getContext "2d") 'rotate) angle)))

(define (save-context)
  (jslet ((canvas::object (current-canvas)))
    ((ref (canvas.getContext "2d") 'save)))
  (let1 ctx2 (copy-context2d (slot-ref (current-canvas) 'context2d))
    (push! (slot-ref (current-canvas) 'context2d-list) ctx2)))

(define (scale x y)
  (jslet ((canvas::object (current-canvas))
          (x::f64)
          (y::f64))
    ((ref (canvas.getContext "2d") 'scale) x y)))

(define (set-transform! a b c d e f)
  (jslet ((canvas::object (current-canvas))
          (a::f64)
          (b::f64)
          (c::f64)
          (d::f64)
          (e::f64)
          (f::f64))
    ((ref (canvas.getContext "2d") 'setTransform) a b c d e f)))

(define (stroke)
  (jslet ((canvas::object (current-canvas)))
    ((ref (canvas.getContext "2d") 'stroke))))

(define (stroke-rect x y w h)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32)
          (w::s32)
          (h::s32))
    ((ref (canvas.getContext "2d") 'strokeRect) x y w h)))

(define (stroke-text text x y :optional (max-width 0))
  (jslet ((canvas::object (current-canvas))
          (text::string)
          (x::s32)
          (y::s32)
          (max-width::s32))
    (if (< 0 max-width)
        ((ref (canvas.getContext "2d") 'strokeText) text x y max-width)
        ((ref (canvas.getContext "2d") 'strokeText) text x y))))

(define (transform a b c d e f)
  (jslet ((canvas::object (current-canvas))
          (a::f64)
          (b::f64)
          (c::f64)
          (d::f64)
          (e::f64)
          (f::f64))
    ((ref (canvas.getContext "2d") 'transform) a b c d e f)))

(define (translate x y)
  (jslet ((canvas::object (current-canvas))
          (x::s32)
          (y::s32))
    ((ref (canvas.getContext "2d") 'translate) x y)))

(define (create-image-data w h)
  (let1 image (make <graviton-image> :width w :height h)
    (jslet ((canvas::object (current-canvas))
            (image*::object* image)
            (w::s32)
            (h::s32))
      (let ((image ((ref (canvas.getContext "2d") 'createImageData) w h)))
        (set! image*.value image)))
    image))

(define (upload-image-data image data)
  (jslet ((image::object)
          (data::u8vector))
    (image.data.set data)))

(define (download-image-data image)
  (jslet/result ((image::object))
    (result image.data)))
