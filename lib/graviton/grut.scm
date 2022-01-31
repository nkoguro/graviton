;;;
;;; grut.scm - Graviton Utility Toolkit
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

(define-module graviton.grut
  (use graviton)
  (use graviton.app)
  (use graviton.jsffi)
  (use graviton.misc)
  (use text.html-lite)
  (use util.list)
  (use util.match)

  (extend graviton.grut.audio
          graviton.grut.clipboard
          graviton.grut.speech
          graviton.grut.text)

  (export load-image
          load-audio

          alist->style

          grut-canvas-window
          grut-text-window
          grut-text+canvas-window))

(select-module graviton.grut)

(import-js ("/_g/grut/grut.mjs" :as Grut))
(autoload-css "/_g/grut/grut.css")

;;;

(define (load-image url :key (on-error :error))
  (or (jslet/await ((url::string))
        (let ((img (make Image)))
          (set! img.src url)
          (set! img.onload (lambda ()
                             (set! img.onload undefined)
                             (set! img.onerror undefined)
                             (respond img)))
          (set! img.onerror (lambda ()
                              (set! img.onload undefined)
                              (set! img.onerror undefined)
                              (respond #f)))))
      (case on-error
        ((#f) #f)
        ((:error) (errorf "Failed to load image: ~a" url))
        (else
         (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error)))))

(define (load-audio url :key (on-error :error))
  (receive (audio err) (jslet/await ((url::string))
                         (let1 audio (make Audio url)
                           (set! audio.onloadeddata (lambda ()
                                                      (set! audio.onloadeddata undefined)
                                                      (set! audio.onerror undefined)
                                                      (respond audio #f)))
                           (set! audio.onerror (lambda ()
                                                 (set! audio.onloadeddata undefined)
                                                 (set! audio.onerror undefined)
                                                 (respond #f audio.error.message)))))
    (or audio
        (case on-error
          ((#f) #f)
          ((:error) (errorf "Failed to load audio: ~a (~a)" url err))
          (else
           (errorf "bad value for :on-error argument; must be #f or :error, but got ~s" on-error))))))


;;;

(define (alist->style alist)
  (string-join (fold (lambda (pair lst)
                       (if (cdr pair)
                         (cons (format "~a:~a" (car pair) (cdr pair)) lst)
                         lst))
                     '()
                     alist)
               ";"))

(define *min-default-window-width* 800)
(define *min-default-window-height* 600)

(define (min-default-window-size)
  (cond
    ((and (window-context)
          (grv-config-parameter 'iframe-window?))
     (jslet/await ()
       (respond (/ window.innerWidth 3) (/ window.innerHeight 3))))
    (else
     (values *min-default-window-width* *min-default-window-height*))))

(define (fit->css-name fit-type)
  (case fit-type
    ((contain)
     "grut-contain")
    ((cover)
     "grut-cover")
    ((fill)
     "grut-fill")
    ((none)
     "grut-none")
    (else
     (errorf "Invalid fit type: ~s" fit-type))))

(define (grut-canvas-window width height
                            :key
                            (id "canvas")
                            (title #f)
                            (background-color #f)
                            (window-width #f)
                            (window-height #f)
                            (resizable? #t)
                            (fit 'contain)
                            (margin #f))
  (let-values (((default-width default-height) (min-default-window-size)))
    (let* ((window-width (or window-width
                             (and window-height
                                  (ceiling->exact (* window-height (/. width height))))
                             (if (< (/ height default-height) (/ width default-width))
                               (max width default-width)
                               (ceiling->exact (* (max height default-height) (/. width height))))))
           (window-height (or window-height
                              (ceiling->exact (* window-width (/. height width))))))
      (grv-window :body (html:body
                         :style (alist->style `(("background-color" . ,background-color)
                                                ("margin" . 0)
                                                ("width" . "100%")
                                                ("height" . "100%")
                                                ("overflow-x" . "hidden")
                                                ("overflow-y" . "hidden")))
                         (html:canvas :id id :class (fit->css-name fit) :width width :height height
                                      :style (alist->style `(("margin" . ,margin)))))
                  :title title
                  :width window-width
                  :height window-height
                  :resizable? resizable?))))

(define (grut-text-window :key
                          (id "text-console")
                          (title #f)
                          (column #f)
                          (row #f)
                          (font #f)
                          (font-size #f)
                          (color "white")
                          (background-color "black")
                          (window-width #f)
                          (window-height #f)
                          (resizable? #t)
                          (fit 'contain)
                          (scrollbar? #f)
                          (padding #f))
  (grv-window :body (html:body
                     :style (alist->style `(("color" . ,color)
                                            ("background-color" . ,background-color)
                                            ("margin" . 0)
                                            ("width" . "100%")
                                            ("height" . "100%")
                                            ("overflow-x" . "hidden")
                                            ("overflow-y" . "hidden")))
                     (html:grut-text :id id
                                     :class (string-join (list "grut-monospace-font" (fit->css-name fit)) " ")
                                     :column column
                                     :row row
                                     :style (alist->style `(("width" . "100%")
                                                            ("height" . "100%")
                                                            ("box-sizing" . "border-box")
                                                            ("padding" . ,padding)
                                                            ("overflow-y" . ,(if scrollbar?
                                                                               "scroll"
                                                                               "hidden"))
                                                            ("font" . ,font)
                                                            ("font-size" . ,font-size)))))
              :title title
              :width window-width
              :height window-height
              :resizable? resizable?))

(define (grut-text+canvas-window width height
                                 :key
                                 (text-id "text-console")
                                 (canvas-id "canvas")
                                 (column #f)
                                 (row #f)
                                 (font #f)
                                 (font-size #f)
                                 (title #f)
                                 (color "white")
                                 (background-color "black")
                                 (window-width #f)
                                 (window-height #f)
                                 (resizable? #t)
                                 (fit 'contain)
                                 (scrollbar? #f)
                                 (margin #f)
                                 (padding #f))
  (let-values (((default-width default-height) (min-default-window-size)))
    (let* ((window-width (or window-width
                             (and window-height
                                  (ceiling->exact (* window-height (/. width height))))
                             (if (< (/ height default-height) (/ width default-width))
                               (max width default-width)
                               (ceiling->exact (* (max height default-height) (/. width height))))))
           (window-height (or window-height
                              (ceiling->exact (* window-width (/. height width))))))
      (grv-window :body (html:body
                         :style (alist->style `(("color" . ,color)
                                                ("background-color" . ,background-color)
                                                ("margin" . 0)
                                                ("width" . "100%")
                                                ("height" . "100%")
                                                ("overflow-x" . "hidden")
                                                ("overflow-y" . "hidden")))
                         (html:canvas :id canvas-id :class (fit->css-name fit) :width width :height height
                                      :style (alist->style `(("margin" . ,margin))))
                         (html:grut-text :id text-id
                                         :class (string-join (list "grut-monospace-font" (fit->css-name fit)) " ")
                                         :column column
                                         :row row
                                         :style (alist->style `(("width" . "100%")
                                                                ("height" . "100%")
                                                                ("box-sizing" . "border-box")
                                                                ("padding" . ,padding)
                                                                ("overflow-y" . ,(if scrollbar?
                                                                                   "scroll"
                                                                                   "hidden"))
                                                                ("font" . ,font)
                                                                ("font-size" . ,font-size)))))
                  :title title
                  :width window-width
                  :height window-height
                  :resizable? resizable?))))
