;;; canvas.scm - canvas (2D)
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
  (use graviton.browser-objects)
  (use graviton.jsbridge)

  (export <html-canvas-element>
          <canvas-rendering-context-2d>
          <image-data>
          <canvas-gradient>
          <canvas-pattern>
          <text-metrics>
          <dom-matrix>

          f32vector->dom-matrix
          f64vector->dom-matrix
          dom-matrix-copy

          image-data-update!
          ))

(select-module graviton.canvas)

(define-class <html-canvas-element> (<html-element>)
  ((%context-cache :init-form (make-hash-table 'string=?))
   (height :jsproperty "height")
   (width :jsproperty "width"))
  :jsclass "HTMLCanvasElement")

(define-jsobject-method <html-canvas-element> get-context (context-type :optional (context-attribute '()))
  ;; Canvas.getContext returns the same object as was returned the last time the method was invoked with the same first argument.
  ;; https://html.spec.whatwg.org/multipage/canvas.html#dom-canvas-getcontext
  (let1 cache (~ self'%context-cache)
    (or (hash-table-get cache context-type #f)
        (let1 ctx (jslet/result ((canvas::object self)
                                 (context-type::string)
                                 (context-attribute::json))
                    (result (canvas.getContext context-type context-attribute)))
          (hash-table-put! cache context-type ctx)
          ctx))))

(define-automatic-jsobject-methods <html-canvas-element>
  ("captureStream" :result)
  ("toDataURL" :result))


(define-class <canvas-rendering-context-2d> (<jsobject>)
  ((canvas :jsproperty "canvas"
           :read-only? #t
           :cacheable? #t)
   (fill-style :jsproperty "fillStyle")
   (font :jsproperty "font")
   (global-alpha :jsproperty "globalAlpha")
   (global-composite-operation :jsproperty "globalCompositeOperation")
   (image-smoothing-enabled :jsproperty "imageSmoothingEnabled")
   (image-smoothing-quality :jsproperty "imageSmoothingQuality")
   (line-cap :jsproperty "lineCap")
   (line-dash-offset :jsproperty "lineDashOffset")
   (line-join :jsproperty "lineJoin")
   (line-width :jsproperty "lineWidth")
   (miter-limit :jsproperty "miterLimit")
   (shadow-blur :jsproperty "shadowBlur")
   (shadow-color :jsproperty "shadowColor")
   (shadow-offset-x :jsproperty "shadowOffsetX")
   (shadow-offset-y :jsproperty "shadowOffsetY")
   (stroke-style :jsproperty "strokeStyle")
   (text-align :jsproperty "textAlign")
   (text-baseline :jsproperty "textBaseline"))
  :jsclass "CanvasRenderingContext2D")

(define-automatic-jsobject-methods <canvas-rendering-context-2d>
  "arc"
  "arcTo"
  "beginPath"
  "bezierCurveTo"
  "clearRect"
  "clip"
  "closePath"
  ("createImageData" :result)
  ("createLinearGradient" :result)
  ("createPattern" :result)
  ("createRadialGradient" :result)
  "drawFocusIfNeeded"
  "drawImage"
  "ellipse"
  "fill"
  "fillRect"
  "fillText"
  "getImageData"
  ("getImageData" :result)
  ("getLineDash" :result)
  ("getTransform" :result)
  ("isPointInPath" :result)
  ("isPointInStroke" :result)
  "lineTo"
  ("measureText" :result)
  "moveTo"
  "putImageData"
  "quadraticCurveTo"
  "rect"
  "restore"
  "rotate"
  "save"
  "scale"
  "setLineDash"
  "setTransform"
  "stroke"
  "strokeRect"
  "strokeText"
  "transform"
  "translate"
  )


(define-class <image-data> (<jsobject>)
  ((data :jsproperty "data"
         :read-only? #t)
   (height :jsproperty "height"
           :read-only? #t
           :cacheable? #t)
   (width :jsproperty "width"
          :read-only? #t
          :cacheable? #t))
  :jsclass "ImageData")

(define (image-data-update! image data)
  (jslet ((image::object)
          (data::u8vector))
    (image.data.set data)))


(define-class <canvas-gradient> (<jsobject>)
  ()
  :jsclass "CanvasGradient")

(define-automatic-jsobject-methods <canvas-gradient>
  "addColorStop")


(define-class <canvas-pattern> (<jsobject>)
  ()
  :jsclass "CanvasPattern")

(define-automatic-jsobject-methods <canvas-pattern>
  "setTransform")


(define-class <dom-matrix> (<jsobject>)
  ((is-2d :jsproperty "is2D"
          :read-only? #t)
   (is-identity :jsproperty "isIdentity"
                :read-only? #t)
   (m11 :jsproperty "m11")
   (m12 :jsproperty "m12")
   (m13 :jsproperty "m13")
   (m14 :jsproperty "m14")
   (m21 :jsproperty "m21")
   (m22 :jsproperty "m22")
   (m23 :jsproperty "m23")
   (m24 :jsproperty "m24")
   (m31 :jsproperty "m31")
   (m32 :jsproperty "m32")
   (m33 :jsproperty "m33")
   (m34 :jsproperty "m34")
   (m41 :jsproperty "m41")
   (m42 :jsproperty "m42")
   (m43 :jsproperty "m43")
   (m44 :jsproperty "m44")
   (a :jsproperty "a")
   (b :jsproperty "b")
   (c :jsproperty "c")
   (d :jsproperty "d")
   (e :jsproperty "e")
   (f :jsproperty "f"))
  :jsclass "DOMMatrix")

(define-automatic-jsobject-methods <dom-matrix>
  ("invertSelf" :result)
  ("multiplySelf" :result)
  ("preMultiplySelf" :result)
  ("translateSelf" :result)
  ("scaleSelf" :result)
  ("scale3dSelf" :result)
  ("rotateSelf" :result)
  ("rotateAxisAngleSelf" :result)
  ("rotateFromVectorSelf" :result)
  ("setMatrixValue" :result)
  ("skewXSelf" :result)
  ("skewYSelf" :result))

(define (f32vector->dom-matrix f32vec)
  (unless (f32vector? f32vec)
    (errorf "<f32vector> required, but got ~s" f32vec))
  (jslet/result ((f32vec))
    (result (DOMMatrix.fromFloat32Array f32vec))))

(define (f64vector->dom-matrix f64vec)
  (unless (f64vector? f64vec)
    (errorf "<f64vector> required, but got ~s" f64vec))
  (jslet/result ((f64vec))
    (result (DOMMatrix.fromFloat64Array f64vec))))

(define (dom-matrix-copy dom-matrix)
  (jslet/result ((dom-matrix::object))
    (result (DOMMatrix.fromMatrix dom-matrix))))


(define-class <text-metrics> (<jsobject>)
  ((actual-bounding-box-ascent :jsproperty "actualBoundingBoxAscent"
                               :read-only? #t
                               :cacheable? #t)
   (actual-bounding-box-descent :jsproperty "actualBoundingBoxDescent"
                                :read-only? #t
                                :cacheable? #t)
   (actual-bounding-box-left :jsproperty "actualBoundingBoxLeft"
                             :read-only? #t
                             :cacheable? #t)
   (actual-bounding-box-right :jsproperty "actualBoundingBoxRight"
                              :read-only? #t
                              :cacheable? #t)
   (alphabetic-baseline :jsproperty "alphabeticBaseline"
                        :read-only? #t
                        :cacheable? #t)
   (em-height-ascent :jsproperty "emHeightAscent"
                     :read-only? #t
                     :cacheable? #t)
   (em-height-descent :jsproperty "emHeightDescent"
                      :read-only? #t
                      :cacheable? #t)
   (font-bounding-box-ascent :jsproperty "fontBoundingBoxAscent"
                             :read-only? #t
                             :cacheable? #t)
   (font-bounding-box-descent :jsproperty "fontBoundingBoxDescent"
                              :read-only? #t
                              :cacheable? #t)
   (hanging-baseline :jsproperty "hangingBaseline"
                     :read-only? #t
                     :cacheable? #t)
   (ideographic-baseline :jsproperty "ideographicBaseline"
                         :read-only? #t
                         :cacheable? #t)
   (width :jsproperty "width"
          :read-only? #t
          :cacheable? #t))
  :jsclass "TextMetrics")
