;;;
;;; browser-objects.scm - Browser Objects
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

(define-module graviton.browser-objects
  (use graviton.app)
  (use graviton.comm)
  (use graviton.jsffi)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)

  (export <jspromise>
          <jsfunction>
          apply-jsfunction
          bind-jsfunction
          call-jsfunction
          jsfunction->string

          <event-target>
          <node>
          <node-list>
          <css-style-declaration>
          <element>
          <html-element>

          <document>
          document

          <html-body-element>

          <html-image-element>

          <window>
          window

          <screen>
          <blob>
          <html-media-element>
          <html-audio-element>
          <html-video-element>
          ))

(select-module graviton.browser-objects)

;;; Promise

(define-class <jspromise> (<jsobject>)
  ()
  :jsclass "Promise")

(define-automatic-jsobject-methods <jspromise>
  "catch"
  "then"
  "finally")

;;; Function

(define-class <jsfunction> (<jsobject>)
  ((length :jsproperty "length"
           :read-only? #t
           :cacheable? #t)
   (name :jsproperty "name"
         :read-only? #t
         :cacheable? #t))
  :jsclass "Function")

(define-automatic-jsobject-methods <jsfunction>
  ("apply" :result)
  ("bind" :result)
  ("call" :result)
  ("toString" :result))

(define-method apply-jsfunction ((jsfunc <jsfunction>) this-arg args)
  ((~ <jsfunction> 'apply) jsfunc this-arg (list->vector args)))

(define-method bind-jsfunction ((jsfunc <jsfunction>) this-arg :rest args)
  (apply (~ <jsfunction> 'bind) jsfunc this-arg args))

(define-method call-jsfunction ((jsfunc <jsfunction>) this-arg :rest args)
  (apply (~ <jsfunction> 'call) jsfunc this-arg args))

(define-method object-apply ((jsfunc <jsfunction>) :rest args)
  (apply-jsfunction jsfunc 'null args))

(define-method jsfunction->string ((jsfunc <jsfunction>))
  ((~ <jsfunction> 'to-string) jsfunc))

;;; EventTarget

(define-class <event-target> (<jsobject>)
  ()
  :jsclass "EventTarget")

(define-automatic-jsobject-methods <event-target>
  "addEventListener"
  "removeEventListener"
  "dispatchEvent")

;;; Node

(define-class <node> (<event-target>)
  ((base-uri :jsproperty "baseURI"
             :read-only? #t)
   (child-nodes :jsproperty "childNodes"
                :read-only? #t)
   (first-child :jsproperty "firstChild"
                :read-only? #t)
   (is-connected :jsproperty "isConnected"
                 :read-only? #t)
   (last-child :jsproperty "lastChild"
               :read-only? #t)
   (next-sibling :jsproperty "nextSibling"
                 :read-only? #t)
   (node-name :jsproperty "nodeName"
              :read-only? #t
              :cacheable? #t)
   (node-type :jsproperty "nodeType"
              :read-only? #t
              :cacheable? #t)
   (node-value :jsproperty "nodeValue")
   (owner-document :jsproperty "ownerDocument"
                   :read-only? #t)
   (parent-node :jsproperty "parentNode"
                :read-only? #t)
   (parent-element :jsproperty "parentElement"
                   :read-only? #t)
   (previous-sibling :jsproperty "previousSibling"
                     :read-only? #t)
   (text-content :jsproperty "textContent"))
  :jsclass "Node")

(define-automatic-jsobject-methods <node>
  ("appendChild" :result)
  ("cloneNode" :result)
  ("compareDocumentPosition" :result)
  ("contains" :result)
  ("getRootNode" :result)
  ("hasChildNodes" :result)
  ("insertBefore" :result)
  ("isDefaultNamespace" :result)
  ("isEqualNode" :result)
  ("isSameNode" :result)
  ("lookupPrefix" :result)
  ("lookupNamespaceURI" :result)
  "normalize"
  "removeChild"
  ("replaceChild" :result))

(define-class <node-list> (<jsobject>)
  ((length :jsproperty "length"))
  :jsclass "NodeList")

(define-automatic-jsobject-methods <node-list>
  ("item" :result)
  ("entries" :result)
  "forEach"
  ("keys" :result)
  ("values" :result))

;;; CSS style declaration

(define-class <css-style-declaration> (<jsobject>)
  ((css-text :jsproperty "cssText")
   (length :jsproperty "length"
           :read-only? #t)
   (parent-rule :jsproperty "parentRule"
                :read-only? #t))
  :jsclass "CSSStyleDeclaration")

(define-automatic-jsobject-methods <css-style-declaration>
  ("getPropertyPriority" :result)
  ("getPropertyValue" :result)
  ("item" :result)
  ("removeProperty" :result)
  "setProperty")

;;; HTML Element

(define-class <element> (<node>)
  ((attributes :jsproperty "attributes"
               :read-only? #t
               :cacheable? #t)
   (class-list :jsproperty "classList"
               :read-only? #t
               :cacheable? #t)
   (class-name :jsproperty "className")
   (client-height :jsproperty "clientHeight"
                  :read-only? #t)
   (client-left :jsproperty "clientLeft"
                :read-only? #t)
   (client-top :jsproperty "clientTop"
               :read-only? #t)
   (client-width :jsproperty "clientWidth"
                 :read-only? #t)
   (computed-name :jsproperty "computedName"
                  :read-only? #t)
   (computed-role :jsproperty "computedRole"
                  :read-only? #t)
   (id :jsproperty "id")
   (inner-html :jsproperty "innerHTML")
   (local-name :jsproperty "localName"
               :read-only? #t)
   (namespace-uri :jsproperty "namespaceURI"
                  :read-only? #t)
   (next-element-sibling :jsproperty "nextElementSibling"
                         :read-only? #t)
   (outer-html :jsproperty "outerHTML")
   (part :jsproperty "part")
   (prefix :jsproperty "prefix"
           :read-only? #t)
   (previous-element-sibling :jsproperty "previousElementSibling"
                             :read-only? #t)
   (scroll-height :jsproperty "scrollHeight"
                  :read-only? #t)
   (scroll-left :jsproperty "scrollLeft")
   (scroll-top :jsproperty "scrollTop")
   (scroll-width :jsproperty "scrollWidth"
                 :read-only? #t)
   (shadow-root :jsproperty "shadowRoot"
                :read-only? #t)
   (slot :jsproperty "slot")
   (tag-name :jsproperty "tagName"
             :read-only? #t
             :cacheable? #t)
   (assigned-slot :jsproperty "assignedSlot"
                  :read-only? #t))
  :jsclass "Element")

(define-automatic-jsobject-methods <element>
  ("attachShadow" :result)
  ("animate" :result)
  ("closest" :result)
  ("getAttribute" :result)
  ("getAttributeNames" :result)
  ("getAttributeNS" :result)
  ("getBoundingClientRect" :result)
  ("getClientRects" :result)
  ("getElementsByClassName" :result)
  ("getElementsByTagName" :result)
  ("getElementsByTagNameNS" :result)
  ("hasAttribute" :result)
  ("hasAttributeNS" :result)
  ("hasAttributes" :result)
  ("hasPointerCapture" :result)
  "insertAdjacentElement"
  "insertAdjacentHTML"
  ("matches" :result)
  ("querySelector" :result)
  ("querySelectorAll" :result)
  "releasePointerCapture"
  "remove"
  "removeAttribute"
  "removeAttributeNS"
  ("requestFullscreen" :result)
  "requestPointerLock"
  "scroll"
  "scrollBy"
  "scrollIntoView"
  "scrollTo"
  "setAttribute"
  "setAttributeNS"
  "setPointerCapture"
  ("toggleAttribute" :result))

(define-class <html-element> (<element>)
  ((access-key :jsproperty "accessKey")
   (access-key-label :jsproperty "accessKeyLabel"
                     :read-only? #t)
   (content-editable :jsproperty "contentEditable")
   (is-content-editable :jsproperty "isContentEditable"
                        :read-only? #t)
   (dataset :jsproperty "dataset"
            :read-only? #t
            :cacheable? #t)
   (dir :jsproperty "dir")
   (draggable :jsproperty "draggable")
   (dropzone :jsproperty "dropzone"
             :read-only? #t)
   (hidden :jsproperty "hidden")
   (inner-text :jsproperty "innerText")
   (lang :jsproperty "lang")
   (nonce :jsproperty "nonce")
   (offset-height :jsproperty "offsetHeight"
                  :read-only? #t)
   (offset-left :jsproperty "offsetLeft"
                :read-only? #t)
   (offset-parent :jsproperty "offsetParent"
                  :read-only? #t)
   (offset-top :jsproperty "offsetTop"
               :read-only? #t)
   (offset-width :jsproperty "offsetWidth"
                 :read-only? #t)
   (style :jsproperty "style"
          :read-only? #t
          :cacheable? #t)
   (tab-index :jsproperty "tabIndex")
   (title :jsproperty "title"))
  :jsclass "HTMLElement")

(define-automatic-jsobject-methods <html-element>
  "blur"
  "click"
  "focus")

(define-class <document> (<node>)
  ((anchors :jsproperty "anchors"
            :read-only? #t)
   (body :jsproperty "body")
   (character-set :jsproperty "characterSet"
                  :read-only? #t)
   (compat-mode :jsproperty "compatMode"
                :read-only? #t)
   (content-type :jsproperty "contentType"
                 :read-only? #t)
   (doc-type :jsproperty "docType"
             :read-only? #t)
   (document-element :jsproperty "documentElement"
                     :read-only? #t)
   (document-uri :jsproperty "documentURI"
                 :read-only? #t)
   (embeds :jsproperty "embeds"
           :read-only? #t)
   (fonts :jsproperty "fonts")
   (forms :jsproperty "forms"
          :read-only? #t)
   (head :jsproperty "head"
         :read-only? #t)
   (hidden :jsproperty "hidden"
           :read-only? #t)
   (images :jsproperty "images"
           :read-only? #t)
   (implementation :jsproperty "implementation"
                   :read-only? #t)
   (links :jsproperty "links"
          :read-only? #t)
   (picture-in-picture-enabled :jsproperty "pictureInPictureEnabled"
                               :read-only? #t)
   (plugins :jsproperty "plugins"
            :read-only? #t)
   (scripts :jsproperty "scripts"
            :read-only? #t)
   (scrolling-element :jsproperty "scrollingElement"
                      :read-only? #t)
   (timeline :jsproperty "timeline"
             :read-only? #t)
   (visibility-state :jsproperty "visibilityState"
                     :read-only? #t)
   (child-element-count :jsproperty "childElementCount"
                        :read-only? #t)
   (children :jsproperty "children"
             :read-only? #t)
   (first-element-child :jsproperty "firstElementChild"
                        :read-only? #t)
   (last-element-child :jsproperty "lastElementChild"
                       :read-only? #t)
   (cookie :jsproperty "cookie")
   (default-view :jsproperty "defaultView" :read-only? #t)
   (design-mode :jsproperty "designMode")
   (dir :jsproperty "dir"
        :read-only? #t)
   (domain :jsproperty "domain")
   (last-modified :jsproperty "lastModified"
                  :read-only? #t)
   (location :jsproperty "location"
             :read-only? #t)
   (ready-state :jsproperty "readyState"
                :read-only? #t)
   (referrer :jsproperty "referrer"
             :read-only? #t)
   (title :jsproperty "title")
   (url :jsproperty "URL"
        :read-only? #t)
   (active-element :jsproperty "activeElement"
                   :read-only? #t)
   (fullscreen-element :jsproperty "fullscreenElement"
                       :read-only? #t)
   (pointer-lock-element :jsproperty "pointerLockElement"
                         :read-only? #t)
   (stylesheets :jsproperty "stylesheets"
                :read-only? #t))
  :jsclass "Document")

(define-global-jsobject document (jslet/await ()
                                   (respond document)))

(define-automatic-jsobject-methods <document>
  ("adoptNode" :result)
  ("createAttribute" :result)
  ("createAttributeNS" :result)
  ("createCDATASection" :result)
  ("createComment" :result)
  ("createDocumentFragment" :result)
  ("createElement" :result)
  ("createElementNS" :result)
  ("createNodeIterator" :result)
  ("createProcessingInstruction" :result)
  ("createRange" :result)
  ("createTextNode" :result)
  ("createTreeWalker" :result)
  ("exitPictureInPicture" :result)
  "exitPointerLock"
  ("getElementsByClassName" :result)
  ("getElementsByTagName" :result)
  ("getElementsByTagNameNS" :result)
  ("hasStorageAccess" :result)
  ("importNode" :result)
  ("requestStorageAccess" :result)
  ("getElementById" :result)
  ("querySelector" :result)
  ("createExpression" :result)
  ("createNSResolver" :result)
  ("evaluate" :result)
  "close"
  ("getElementsByName" :result)
  ("hasFocus" :result)
  ("open" :result)
  "write"
  "writeln")

(define-jsobject-method <document> query-selector-all (selectors)
  (vector->list (jslet/await ((document::object self)
                              (selectors::string))
                  (respond (Array.from (document.querySelectorAll selectors))))))

;;; Document Body

(define-class <html-body-element> (<html-element>)
  ()
  :jsclass "HTMLBodyElement"
  :jsobject-method-prefix "body")

;;;

(define-class <html-image-element> (<html-element>)
  ((alt :jsproperty "alt")
   (complete :jsproperty "complete"
             :read-only? #t)
   (cross-origin :jsproperty "crossOrigin")
   (current-src :jsproperty "currentSrc"
                :read-only? #t)
   (decoding :jsproperty "decoding")
   (height :jsproperty "height")
   (is-map :jsproperty "isMap")
   (loading :jsproperty "loading")
   (natural-height :jsproperty "naturalHeight"
                   :read-only? #t)
   (natural-width :jsproperty "naturalWidth"
                  :read-only? #t)
   (referrer-policy :jsproperty "referrerPolicy")
   (sizes :jsproperty "sizes")
   (src :jsproperty "src")
   (srcset :jsproperty "srcset")
   (use-map :jsproperty "useMap")
   (width :jsproperty "width")
   (x :jsproperty "x"
      :read-only? #t)
   (y :jsproperty "y"
      :read-only? #t))
  :jsclass "HTMLImageElement")

(define-automatic-jsobject-methods <html-image-element>
  "decode")

;;;

(define-class <window> (<event-target>)
  ((closed :jsproperty "closed")
   (console :jsproperty "console"
            :read-only? #t
            :cacheable? #t)
   (custom-elements :jsproperty "customElements"
                    :read-only? #t
                    :cacheable? #t)
   (crypto :jsproperty "crypto")
   (device-pixel-ratio :jsproperty "devicePixelRatio")
   (document :jsproperty "document"
             :read-only? #t)
   (event :jsproperty "event"
          :read-only? #t)
   (frame-element :jsproperty "frameElement"
                  :read-only? #t)
   (frames :jsproperty "frames"
           :read-only? #t)
   (history :jsproperty "history"
            :read-only? #t
            :cacheable? #t)
   (inner-height :jsproperty "innerHeight"
                 :read-only? #t)
   (inner-width :jsproperty "innerWidth"
                :read-only? #t)
   (is-secure-context :jsproperty "isSecureContext"
                      :read-only? #t)
   (length :jsproperty "length"
           :read-only? #t)
   (location :jsproperty "location")
   (locationbar :jsproperty "locationbar"
                :read-only? #t
                :cacheable? #t)
   (local-storage :jsproperty "localStorage"
                  :read-only? #t
                  :cacheable? #t)
   (menubar :jsproperty "menubar"
            :read-only? #t
            :cacheable? #t)
   (name :jsproperty "name")
   (navigator :jsproperty "navigator"
              :read-only? #t
              :cacheable? #t)
   (opener :jsproperty "opener")
   (outer-height :jsproperty "outerHeight"
                 :read-only? #t)
   (outer-width :jsproperty "outerWidth"
                :read-only? #t)
   (page-x-offset :jsproperty "pageXOffset"
                  :read-only? #t)
   (page-y-offset :jsproperty "pageYOffset"
                  :read-only? #t)
   (parent :jsproperty "parent"
           :read-only? #t)
   (performance :jsproperty "performance"
                :read-only? #t
                :cacheable? #t)
   (personalbar :jsproperty "personalbar"
                :read-only? #t
                :cacheable? #t)
   (screen :jsproperty "screen"
           :read-only? #t
           :cacheable? #t)
   (screen-x :jsproperty "screenX"
             :read-only? #t)
   (screen-left :jsproperty "screenLeft"
                :read-only? #t)
   (screen-y :jsproperty "screenY"
             :read-only? #t)
   (screen-top :jsproperty "screenTop"
               :read-only? #t)
   (scrollbars :jsproperty "scrollbars"
               :read-only? #t
               :cacheable? #t)
   (scroll-x :jsproperty "scrollX"
             :read-only? #t)
   (scroll-y :jsproperty "scrollY"
             :read-only? #t)
   (self :jsproperty "self"
         :read-only? #t
         :cacheable? #t)
   (session-storage :jsproperty "sessionStorage")
   (speech-synthesis :jsproperty "speechSynthesis"
                     :read-only? #t
                     :cacheable? #t)
   (status :jsproperty "status")
   (statusbar :jsproperty "statusbar"
              :read-only? #t
              :cacheable? #t)
   (toolbar :jsproperty "toolbar"
            :read-only? #t
            :cacheable? #t)
   (top :jsproperty "top"
        :read-only? #t)
   (visual-viewport :jsproperty "visualViewport"
                    :read-only? #t
                    :cacheable? #t)
   (window :jsproperty "window"
           :read-only? #t
           :cacheable? #t))
  :jsclass "Window")

(define-global-jsobject window (jslet/await ()
                                 (respond window)))

(define-automatic-jsobject-methods <window>
  "alert"
  "blur"
  "close"
  ("confirm" :result)
  "focus"
  ("getComputedStyle" :result)
  ("getSelection" :result)
  ("matchMedia" :result)
  "moveBy"
  "moveTo"
  ("open" :result)
  "postMessage"
  "print"
  ("requestAnimationFrame" :result)
  "resizeBy"
  "resizeTo"
  "scroll"
  "scrollBy"
  "scrollTo"
  "sizeToContent"
  "stop")

(define-class <screen> (<event-target>)
  ((avail-top :jsproperty "availTop")
   (avail-left :jsproperty "availLeft")
   (avail-height :jsproperty "availHeight")
   (avail-width :jsproperty "availWidth")
   (color-depth :jsproperty "colorDepth")
   (height :jsproperty "height")
   (pixel-depth :jsproperty "pixelDepth")
   (width :jsproperty "width"))
  :jsclass "Screen")

;;;

(define-class <blob> (<jsobject>)
  ((size :jsproperty "size"
         :read-only? #t)
   (type :jsproperty "type"
         :read-only? #t))
  :jsclass "Blob")

(define-automatic-jsobject-methods <blob>
  ("arrayBuffer" :result)
  ("slice" :result)
  ("stream" :result)
  ("text" :result))

;;;

(define-class <html-media-element> (<html-element>)
  ((audio-traks :jsproperty "audioTracks")
   (auto-play :jsproperty "autoPlay")
   (buffered :jsproperty "buffered"
             :read-only? #t)
   (controller :jsproperty "controller")
   (controls :jsproperty "controls")
   (controlsList :jsproperty "controlsList"
                 :read-only? #t)
   (cross-origin :jsproperty "crossOrigin")
   (current-src :jsproperty "currentSrc"
                :read-only? #t)
   (current-time :jsproperty "currentTime")
   (default-muted :jsproperty "defaultMuted")
   (default-playback-rate :jsproperty "defaultPlaybackRate")
   (disable-remote-playback :jsproperty "disableRemotePlayback")
   (duration :jsproperty "duration"
             :read-only? #t)
   (ended :jsproperty "ended"
          :read-only? #t)
   (error :jsproperty "error"
          :read-only? #t)
   (loop :jsproperty "loop")
   (media-group :jsproperty "mediaGroup")
   (muted :jsproperty "muted")
   (networkState :jsproperty "network-state"
                 :read-only? #t)
   (paused :jsproperty "paused"
           :read-only? #t)
   (playback-rate :jsproperty "playbackRate")
   (played :jsproperty "played"
           :read-only? #t)
   (preload :jsproperty "preload")
   (ready-state :jsproperty "readyState"
                :read-only? #t)
   (seekable :jsproperty "seekable"
             :read-only? #t)
   (seeking :jsproperty "seeking"
            :read-only? #t)
   (src :jsproperty "src")
   (src-object :jsproperty "srcObject")
   (text-tracks :jsproperty "textTracks"
                :read-only? #t)
   (video-tracks :jsproperty "videoTracks"
                 :read-only? #t)
   (volume :jsproperty "volume"))
  :jsclass "HTMLMediaElement")

(define-automatic-jsobject-methods <html-media-element>
  "addTextTrack"
  ("canPlayType" :result)
  "load"
  "pause"
  "play")

(define-class <html-audio-element> (<html-media-element>)
  ()
  :jsclass "HTMLAudioElement")

(define-class <html-video-element> (<html-media-element>)
  ((height :jsproperty "height")
   (poster :jsproperty "poster")
   (video-height :jsproperty "videoHeight"
                 :read-only? #t)
   (video-width :jsproperty "videoWidth"
                :read-only? #t)
   (width :jsproperty "width")
   (auto-picture-in-picture :jsproperty "autoPictureInPicture")
   (disable-picture-in-picture :jsproperty "disablePictureInPicture"))
  :jsclass "HTMLVideoElement")

(define-automatic-jsobject-methods <html-video-element>
  ("requestPictureInPicture" :result))
