# Graviton

## Table of contents

  * [Overview](#overview)
  * [Installation](#installation)
    * [Note for WSL user](#note-for-wsl-user)
  * [Getting started](#getting-started)
  * [Module: graviton](#module-graviton)
    * [Configuration](#configuration)
    * [Window](#window)
    * [Window parameter](#window-parameter)
    * [Information of the current request](#information-of-the-current-request)
    * [Routing](#routing)
    * [Autoloading CSS](#autoloading-css)
    * [Working with JavaScript](#working-with-javascript)
    * [Worker](#worker)
    * [Asynchronous utilities](#asynchronous-utilities)
    * [Concurrency and Parallelism](#concurrency-and-parallelism)
  * [Module: graviton.grut](#module-gravitongrut)
    * [Audio](#audio)
    * [Clipboard](#clipboard)
    * [Speech](#speech)
    * [Text console](#text-console)
  * [JavaScript FFI and JSiSE (JavaScript in S-Expression)](#javascript-ffi-and-jsise-javascript-in-s-expression)
    * [Literals](#literals)
    * [Vector](#vector)
    * [Object](#object)
    * [Control flow](#control-flow)
    * [Local variables definition](#local-variables-definition)
    * [Assignment](#assignment)
    * [Expression operators](#expression-operators)
    * [Returning values to Scheme](#returning-values-to-scheme)
    * [Global variable definition](#global-variable-definition)


## Overview

Graviton is a library to provide Web-based UI for a standalone Gauche program. You can make UI with HTML, CSS, and JavaScript and integrate the UI into your program.

Graviton also provides JavaScript FFI with S-expression syntax so that you can embed JavaScript code naturally in your program.

You need a modern web browser to access Graviton's Web-based UI. However, Graviton provides an Electron-based client (graviton-player) to interact with the UI. Your program can act as a native GUI application using graviton-player.

CAVEAT: Graviton is a kind of "Web application framework." However, it is designed for one or a few clients. Graviton has no particular restrictions on the number of clients, but it may not be efficient when many clients use it.

## Installation

You need the latest version of Gauche and Gauche-makiki.

```
$ ./configure
$ make
$ make install
```

If you want to install graviton-player, the latest node and npm are required.

```
$ make build-player
$ make install-player
```

### Note for WSL user

If you use WSL, the configure script sets graviton-player architecture to win32-x64. You need to install Wine to build the windows binary on WSL. 

If you want to use linux-x64 graviton-player with WSLg, you need to specify "--with-wsl=" (no args to this option) for the configure script (`./configure --with-wsl=`).


## Getting started

This simple program opens the "Hello, world" window. This window is closed if you press any keys.

```scheme:hello-world.scm
(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body "Hello,world"))
      ()
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))
```

`grv-window` defines a window UI with HTML. `with-window` opens the specified window (or just waits for a request from the web browser if you don't install graviton-player) and invokes a code after the window is opened.

Here is an example of calling JavaScript.

```scheme:hello-world2.scm
(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body (html:div :id "div-block")))
      ()
    ;; Sets "Hello, world" in the div element.
    (let1 div (document'get-element-by-id "div-block")
      (set! (~ div'inner-text) "Hello, world"))
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))
```

Graviton provides `<jsobject>` class, a proxy to a JavaScript object. You can call a method of the object with `(jsobject 'method args ...)` and can access a property using Gauche's slot accessor (e.g. `(slot-ref jsobject 'property)`).

`window` and `document` in Scheme are pre-defined global objects representing `window` and `document` in JavaScript.

NOTE:
You may feel weird about the usage of `document` in the example code. This program can accept multiple requests, so actual JavaScript `document` objects must be different. In fact, `window` and `document` are not `<jsobject>`. They are `<jsobject-provier>` which can return `<jsobject>` in run-time. The returned objects are different in the different connections. You can use `<jsobject-provider>` on behalf of `<jsobject>`.


Here is an example embedding JavaScript code.

```scheme:hello-world3.scm
(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body (html:div :id "div-block")))
      ()
    ;; Sets "Hello, world" in the div element.
    (jslet ((text "Hello, world"))
      (let1 div (document.getElementById "div-block")
        (set! div.innerText text)))
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))
```

This code is equivalent to the previous example. The difference is that JavaScript code sets the text "Hello, world". You can embed a JavaScript code with `(jslet (form ...) body ...)` macro. The body is JavaScript in S-expression. It is translated to an actual JavaScript in compile-time and sent to the client when it accesses this page.

If you want to return values from JavaScript, you can use `(jslet/await (form ...) body ...)` macro.

```scheme:hello-world4.scm
(use graviton)
(use text.html-lite)

(define (main args)
  (with-window (grv-window :body (html:body (html:div :id "div-block")))
      ()
    ;; Sets "Hello, world" in the div element and prints the body in HTML.
    (print (jslet/await ((text "Hello, world"))
             (let1 div (document.getElementById "div-block")
               (set! div.innerText text)
               (respond document.body.innerHTML))))
    ;; Waits for a keyup event.
    (jsevent-await window "keyup" ())
    ;; Closes this window if possible.
    (close-window)))
```

Calling JavaScript is asynchronous, so `jslet/await` doesn't block a thread. `jslet/await` creates a continuation, and the continuation is called after the JavaScript code returns values. Until then, the thread can do other things. For example, there is an event handler in this thread. The handler can run in this thread before the values return.


There are several code examples under `examples/` of this repository. They might be helpful to understand Graviton.

## Module: graviton

### Configuration

<dl>
<dt><code>(grv-config <i>:key host protocol client port access-log error-log iframe-window?</i>)</code></dt>
<dd>
Configures Graviton.
  <dl>
    <dt><code>host</code></dt>
    <dd>
      Hostname of this server. The default is "localhost".
    </dd>
    <dt><code>protocol</code></dt>
    <dd>
      Protocol ("http" or "https") to connect this server. The default is "http".
    </dd>
    <dt><code>mode</code>
    <dd>
      Operation mode of Graviton.
      <dl>
        <dt><code>'browser</code></dt>
        <dd>
          The program opens a main window with Web browser automatically, and it exits if the window is closed. graviton uses the system default Web browser. If you set BROWSER environment variable, the browser is used instead of the system default.  
        </dd>
        <dt><code>'player</code></dt>
        <dd>
          The program opens a main window with graviton-player automatically, and it exits if the window is closed.
        </dd>
        <dt><code>'server</code></dt>
        <dd>
          The program waits for the client requests, and can accept requests from multiple clients.
        </dd>
        <dt><code>#f</code> (default)</dt>
        <dd>
          <code>'player</code> if graviton-player is installed. Otherwise, <code>'browser</code>.
        </dd>
      </dl>
    </dd>
    <dt><code>port</code></dt>
    <dd>
      Port number to receive requests. If you specify 0, a free port will be assigned automatically.
      The default is 0 if the client type is <code>'player</code>, 8080 if the client type is <code>'browser</code>. 
    </dd>
    <dt><code>access-log</code></dt>
    <dt><code>error-log</code></dt>
    <dd>
      The destination of log. <code>#f</code> (no log), <code>#t</code> (stdout), <code><i>string</i></code> (filename) or <code>&lt;log-drain&gt;</code> object.
      The default is <code>#f</code>.
    </dd>
    <dt><code>iframe-window?</code></dt>
    <dd>
      Whether iframe is used or not to open a new window in the program. <code>#t</code> uses iframe, so the new window is opened inside the main window. <code>#f</code> uses <code>window.open</code>, so the new window opens independently of the main window.
      The default is <code>#f</code> if the client type is <code>'player</code>, #t if the client type is <code>'browser</code>.
    </dd>
  </dl>
</dd>

<dt><code>(grv-config-parameter <i>name</i>)</code></dt>
<dd>
Returns the current value of the setting. <i>name</i> must be one of these symbols,
<code>port</code>, <code>host</code>, <code>protocol</code>, <code>client</code>, <code>access-log</code>, <code>error-log</code> or <code>iframe-window?</code>.
</dd>

<dt><code>(client-is-player)</code></dt>
<dd>
Returns <code>#t</code> if the current client is graviton-player.
</dd>

<dt><code>(client-is-browser)</code></dt>
<dd>
Returns <code>#t</code> if the current client is Web browser.
</dd>
</dl>

### Window

When Graviton receives a request from the client, it creates a new window instance and assigns a thread (called "worker") to the window instance. The code running in the worker can operate the associated window only.

<dl>
<dt><code>(grv-window <i>:key title css js head body width height resizable? show?</i>)</code></dt>
<dd>
Returns a window.
  <dl>
    <dt><code>title</code></dt>
    <dd>
      The title of this window.
    </dd>
    <dt><code>css</code></dt>
    <dd>
      CSS used in this window. You can specify the filename or SxCSS. If you want to use multiple css, you can pass them by repeated <code>:css</code> keyword parameters, like <code>(grv-window :css "first.css" :css "second.css")</code>.
    </dd>
    <dt><code>js</code></dt>
    <dd>
      JavaScript filename used in this window. If you want to load multiple JavaScript files, you can pass them by repeated <code>:js</code> keyword parameters.
    </dd>
    <dt><code>head</code></dt>
    <dd>
      &lt;head&gt; element of this window.
    </dd>
    <dt><code>body</code></dt>
    <dd>
      The contents in &lt;body&gt; element of this window.
    </dd>
    <dt><code>width</code></dt>
    <dd>
      The width of this window.
    </dd>
    <dt><code>height</code></dt>
    <dd>
      The height of this window.
    </dd>
    <dt><code>resizable?</code></dt>
    <dd>
      Whether this window is resizable or not.
    </dd>
    <dt><code>show?</code></dt>
    <dd>
      Whether this window is visible or not. The default is <code>#t</code>. This option works only for graviton-player.
    </dd>
  </dl>
</dd>

<dt><code>(with-window <i>window</i> (<i>elements ...</i>) <i>body ...</i>)</code></dt>
<dd>
Opens <i>window</i>, then execute <i>body ...</i> in a newly created worker.
<i>elements ...</i> are IDs of HTML elements, which will be bound to the same names of local variables before the execution. <code>#f</code> will be bound to the variable if the element is not found. 
</dd>

<dt><code>(grv-title)</code></dt>
<dd>
Returns the title of the current window.
</dd>

<dt><code>(grv-title-set! <i>title</i>)</code></dt>
<dd>
Updates the title of the current widnow.
</dd>

<dt><code>(close-window)</code></dt>
<dd>
Closes the current window, and terminates workers related to the window.
</dd>
</dl>



### Window parameter

Graviton provides `<window-parameter>` to hold values per window instance. One `<window-parameter>` object can hold each window instance's value, so you can define it as a global variable and refers to it from each window.

<dl>
<dt><code>(make-window-parameter <i>value ...</i>)</code></dt>
<dd>
Creates <code>&lt;window-parameter&gt;</code> object with the initial values <i>value ...</i>.
</dd>

<dt><code>(make-window-parameter* <i>thunk</i>)</code></dt>
<dd>
Creates <code>&lt;window-parameter&gt;</code> object, but the initialization is delayed until a window instance is created. When a new window instance is created, the <code>&lt;window-parameter&gt;</code> object will be initialized with the return values of <i>thunk</i>.
</dd>

<dt><code>(window-parameter-atomic-ref <i>window-context</i> <i>window-parameter</i> <i>proc</i>)</code></dt>
<dd>
</dd>
Calls <i>proc</i> with the current values of <i>window-parameter</i> in <i>window-context</i>, while locking <i>window-parameter</i>.
</dd>

<dt><code>(window-parameter-atomic-update! <i>window-context</i> <i>window-parameter</i> <i>proc</i>)</code></dt>
<dd>
Calls <i>proc</i> with the current values of <i>window-parameter</i> in <i>window-context</i> while locking <i>window-parameter</i>, and updates the values in <i>window-parameter</i> by the returned values from <i>proc</i>.
</dd>

<dt><code>(window-parameter-ref <i>window-context</i> <i>window-parameter</i>)</code></dt>
<dd>
</dd>
Returns the current values in <i>window-parameter</i>.
</dd>

<dt><code>(window-parameter-set! <i>window-context</i> <i>window-parameter</i> <i>vals...</i>)</code></dt>
<dd>
Sets <i>vals...<i> to <i>window-parameter</i> in <i>window-context</i>.
</dd>

<dt><code>(object-apply (<i>window-parameter</i> &lt;window-parameter&gt;) <i>value ...</i>)</code></dt>
<dd>
Returns the values in <i>window-parameter</i> if no <i>value ...</i> are specified. Otherwise, updates the values in <i>window-parameter</i> with <i>value ...</i>. 

<code>(setter object-apply)</code> is also defined, so you can use it in generalized set!.
</dd>
</dl>

### Information of the current request

<dl>
<dt><code>(query-parameters)</code></dt>
<dd>
Returns query parameters of the current request.
</dd>
<dt><code>(user-agent)</code></dt>
<dd>
Returns User-Agent of the current request.
</dd>
</dl>


### Routing

<dl>
<dt><code>(bind-url-path <i>url-path</i> <i>file-path</i>)</code></dt>
<dt><code>(bind-url-path <i>url-path</i> <i>proc</i>)</code></dt>
<dd>
Binds <i>file-path</i> to <i>url-path</i>, so that the file is accessible with the URL.

You can also bind a procedure <i>proc</i> to <i>url-path</i>. <i>proc</i> is called when the URL is requested, and returns the result of <i>proc</i> as the response. <i>proc</i> must returns a body and the content-type. 
</dd>

<dt><code>(file->url <i>filename</i> :optional <i>content-type</i>)</code></dt>
<dd>
Allocates an URL for the specified <i>filename</i>. If the URL is already allocated for the file, the same URL returns.

If <i>content-type</i> is omitted, it will be estimated from <i>filename</i>.
</dd>

<dt><code>(data->url <i>data</i> <i>content-type</i>)</code></dt>
<dd>
Allocates an URL for the <i>data</i>.
</dd>

<dt><code>(json->url <i>json</i>)</code></dt>
<dd>
Allocates an URL for the <i>json</i>. <i>json</i> must be a list or a vector.
</dd>

<dt><code>(sxml->url <i>sxml</i>)</code></dt>
<dd>
Allocates an URL for the <i>sxml</i>.
</dd>
</dl>



### Autoloading CSS

<dl>
<dt><code>(autoload-css <i>css-path ...</i>)</code></dt>
<dd>
Specifies CSS paths <i>css-path ...</i> to be loaded in all window instances.
</dd>
</dl>


### Working with JavaScript

<code>&lt;jsobject&gt;</code> represents a JavaScript object. `jslet/await` can return the object if the returned value is a non-basic JavaScript object. You can call the object's method and access the object's property with it. You can also pass the object to JavaScript world with `jslet` again. 

Graviton defines several JavaScript classes (see below for the pre-defined class list). You can use "kebab-case" symbol as method and property name for the pre-defined classes. 
For example, you can call `document.getElementById(...)` using `(document'get-element-by-id ...)` and refer `document.innerHTML` using `(slot-ref document 'inner-html)`. 

<dl>
<dt><code>(object-apply (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>method</i> <i>&lt;symbol&gt;</i>) <i>arg ...</i>)</code></dt>
<dd>
Calls <i>method</i> of <i>jsobj</i>. <i>method</i> is a kebab-case symbol.
</dd>

<dt><code>(object-apply (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>method</i> <i>&lt;string&gt;</i>) [:result] <i>arg ...</i>)</code></dt>
<dd>
Calls <i>method</i> of <i>jsobj</i>, but <i>method</i> is the original JavaScript method name. You can use this style for a non-predefined JavaScript class's method call. If you want to get the result of the method, you need to set <code>:result</code> keyword.
</dd>

<br>

<dt><code>(ref (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>property</i> <i>&lt;symbol&gt;</i>))</code></dt>
<dt><code>((setter ref) (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>property</i> <i>&lt;symbol&gt;</i>) <i>value</i>)</code></dt>
<dd>
Same as <code>(slot-ref <i>jsobj</i> <i>property</i>)</code> and <code>(slot-set! <i>jsobj</i> <i>property</i> <i>value</i>)</code>. <i>property</i> is a kebab-case symbol.
</dd>

<dt><code>(ref (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>property-name</i> <i>&lt;string&gt;</i>))</code></dt>
<dt><code>((setter ref) (<i>jsobj</i> <i>&lt;jsobject&gt;</i>) (<i>property-name</i> <i>&lt;string&gt;</i>) <i>value</i>)</code></dt>
<dd>
Refers the object property with the original JavaScript property name. You can use them for an object of non-predefined JavaScript classes. For example, you can access <code>foo.barBaz</code> with <code>(ref foo "barBaz")</code>.  
</dd>
<dl>
</dl>

Graviton defines these JavaScript objects as global.
- `window` refers to `window` in JavaScript.
- `document` refers to `document` in JavaScript.
- `audio-context` refers to an instance of `AudioContext`. This object is automatically created by Graviton. You can use it for WebAudio API.

NOTE: In the current browser implementation, AudioContext is initially in the "suspended" state and must be enabled by user interaction. You don't need to implement it because Graviton takes care of this operation.

#### Pre-defined JavaScript classes

These classes inherit `<jsobject>`, and their hierarchy is the same as the corresponding JavaScript class hierarchy.

- `<event-target>` for `EventTarget`
- `<node>` for `Node`
- `<node-list>` for `NodeList`
- `<css-style-declaration>` for `CSSStyleDeclaration`
- `<element>` for `Element`
- `<html-element>` for `HTMLElement`
- `<document>` for `Document`
- `<html-body-element>` for `HTMLBodyElement`
- `<html-image-element>` for `HTMLImageElement`
- `<window>` for `Window`
- `<screen>` for `Screen`
- `<blob>` for `Blob`
- `<html-media-element>` for `HTMLMediaElement`
- `<html-audio-element>` for `HTMLAudioElement`
- `<html-video-element>` for `HTMLVideoElement`
- `<event>` for `Event`
- `<html-canvas-element>` for `HTMLCanvasElement`
- `<canvas-rendering-context>` for `CanvasRenderingContext`
- `<image-data>` for `ImageData`
- `<canvas-gradient>` for `CanvasGradient`
- `<canvas-pattern>` for `CanvasPattern`
- `<text-metrics>` for `TextMetrics`
- `<dom-matrix>` for `DOMMatrix`
- `<base-audio-context>` for `BaseAudioContext`
- `<audio-context>` for `AudioContext`
- `<offline-audio-context>` for `OfflineAudioContext`
- `<audio-node>` for `AudioNode`
- `<analyser-node>` for `AnalyserNode`
- `<audio-buffer>` for `AudioBuffer`
- `<audio-scheduled-source-node>` for `AudioScheduledSourceNode`
- `<audio-buffer-source-node>` for `AudioBufferSourceNode`
- `<audio-destination-node>` for `AudioDestinationNode`
- `<audio-listener>` for `AudioListener`
- `<audio-param>` for `AudioParam`
- `<biquad-filter-node>` for `BiquadFilterNode`
- `<channel-merger-node>` for `ChannelMergerNode`
- `<channel-splitter-node>` for `ChannelSplitterNode`
- `<constant-source-node>` for `ConstantSourceNode`
- `<convolver-node>` for `ConvolverNode`
- `<delay-node>` for `DelayNode`
- `<dynamic-compressor-node>` for `DynamicCompressorNode`
- `<gain-node>` for `GainNode`
- `<iir-filter-node>` for `IIRFilterNode`
- `<media-element-audio-source-node>` for `MediaElementAudioSourceNode`
- `<media-stream-audio-destination-node>` for `MediaStreamAudioDestinationNode`
- `<media-stream-audio-source-node>` for `MediaStreamAudioSourceNode`
- `<offline-audio-completion-event>` for `OfflineAudioCompletionEvent`
- `<oscillator-node>` for `OscillatorNode`
- `<panner-node>` for `PannerNode`
- `<periodic-wave>` for `PeriodicWave`
- `<stereo-panner-node>` for `StereoPannerNode`
- `<wave-shaper-node>` for `WaveShaperNode`

#### Canvas utilities

Graviton provides these functions for Canvas.

<dl>
<dt><code>(f32vector->dom-matrix <i>f32vec</i>)</code></dt>
<dd>
Converts <code>&lt;f32vector&gt;</code> to <code>&lt;dom-matrix&gt;</code>.
</dd>

<dt><code>(f64vector->dom-matrix <i>f64vec</i>)</code></dt>
<dd>
Converts <code>&lt;f64vector&gt;</code> to <code>&lt;dom-matrix&gt;</code>.
</dd>

<dt><code>(dom-matrix-copy <i>dom-matrix</i>)</code></dt>
<dd>
Copies <code>&lt;dom-matrix&gt;</code> object.
</dd>

<dt><code>(image-data-update! <i>image-data</i> <i>data</i>)</code></dt>
<dd>
Updates the data of <i>image-data</i> with <i>data</i>. <i>image-data</i> is a instance of <code>&lt;image-data&gt;</code>, and <i>data</i> must be <code>&lt;u8vector&gt;</code>.

This function is identical to <code>imageData.data.set(data)</code> in JavaScript. Writing this property-method chain is wordy in the current Graviton JavaScript object syntax. It is provided for your convenience.
</dd>
</dl>

#### JavaScript event utilities

You can use `(event-target'add-event-listener ...)` for JavaScript event handling. A scheme procedure can be passed to the listener, so you can catch the event in Scheme world.

However, the procedure will be called with a JavaScript event object. If you need a value in the event, it needs addtional JavaScript FFI requests and needs to wait for the response. Graviton provides several event utilities to reduce such FFI request/response.

<dl>
<dt><code>(jsevent-callback-set! (<i>jsobj</i> &lt;jsobject&gt;) <i>event-type</i> <i>prop-specs</i> <i>proc</i> :key <i>(use-capture? #f)</i>)</code></dt>
<dd>
Sets a listener <i>proc</i> to <i>event-type</i> of <i>jsobj</i>. You can specify a list of properties with <i>prop-specs</i>. <i>proc</i> will be called with the values of the properties, so that no additional JavaScript FFI request/response are needed.

<i>proc-specs</i> must be a list of property names. For example, '("code" "key") means event.code and event.key. These values are passed to <i>proc</i>. You can also use a property chain, like '("relatedTarget.id"), an index reference, like '("results[0]"), or their combination.

<i>use-capture?</i> is the same to <code>EventTarget.addEventListener</code>. If you want to capture the event in bubbling phase, sets it to <code>#t</code>.
</dd>

<dt><code>(jsevent-callback-delete! (<i>jsobj</i> &lt;jsobject&gt;) <i>event-type</i> :key <i>(use-capture? #f)</i>)</code></dt>
<dd>
Deletes the listener of the specified event.
</dd>

<dt><code>(on-jsevent <i>jsobj</i> <i>event-type</i> (<i>arg ...</i>) <i>body ...</i>)</code></dt>
<dt><code>(on-jsevent <i>jsobj</i> (<i>event-type</i> :use-capture? <i>use-capture?</i>) (<i>arg ...</i>) <i>body ...</i>)</code></dt>
<dd>
This is a convenient macro to set an event listener. When an event is triggered, the properties of the event are bound to <i>arg ...</i>, then <i>body ...</i> are executed.

The property of the event will be bound to corresponding <i>arg</i>. For example, <code>event.key</code> will be bound to <i>arg</i> if <i>arg</i> is <code>key</code>. 
You can also use a kebab-case symbol like <code>related-target</code>. <code>event.relatedTarget</code> will be bound in this case.
If you want to specify the property name explicitly, use <code>(<i>arg</i> <i>"eventName"</i>)</code> style.
</dd>

<dt><code>(request-animation-frame-callback! <i>proc</i>)</code></dt>
<dd>
Registers <i>proc</i> as a callback before repaint. <i>proc</i> takes one argument, the current time in millisecond. 

This function is equivalent to <code>window.requestAnimationFrame(<i>proc</i>)</code>.
</dd>

<dt><code>(cancel-animation-frame-callback! <i>proc</i>)</code></dt>
<dd>
Removes <i>proc</i> from repaint callback. 
</dd>

<dt><code>(on-animation-frame (<i>arg</i>) <i>body ...</i>)</code></dt>
<dt><code>(on-animation-frame :priority <i>priority</i> (<i>arg</i>) <i>body ...</i>)</code></dt>
<dd>
This is a convenient macro to register an animation frame callback.
</dd>

<dt><code>(jsevent-await <i>jsobj</i> <i>event-type</i> <i>prop-specs</i> :key <i>use-capture?</i>)</code></dt>
<dd>
Waits for <i>event-type</i> of <i>jsobj</i>, and returns the properties of the event, which are specified by <i>prop-specs</i>.

If prop-specs is <code>'()</code>, returns <code>#&lt;undef&gt;</code>.
</dd>
</dl>



### Worker

Graviton provides Worker (`<grv-worker>`) to support an asynchronous mechanism. Worker can run code, and the code can yield its execution to other code.
One worker is created when a window is opened. It is called "main worker". You can create a worker yourselves to run code in the background.

Worker also has a messaging mechanism. Each worker can communicate with other workers.

<dl>
<dt><code>(make-worker <i>thunk</i> :key <i>name</i> <i>size</i>)</code></dt>
<dd>
Makes new worker. <i>thunk</i> will run when the worker starts.

You can specify these keyword parameters.
  <dl>
    <dt><code>name</code></dt>
    <dd>
      The name of the worker.
    </dd>
    <dt><code>size</code></dt>
    <dd>
      The number of threads. The default is 1.
    </dd>
  </dl>
</dd>

<dt><code>(worker-run <i>worker</i>)</code></dt>
<dd>
Executes <i>worker</i>.
</dd>

<dt><code>(grv-worker [:name <i>name</i>] [:size <i>size</i>] <i>body ...</i>)</code></dt>
<dd>
This is a handy macro to make and run new worker. The keyword parameters are the same as <code>make-worker</code>. 
</dd>

<dt><code>(current-worker)</code></dt>
<dd>
Returns the current worker in which the current code runs.
</dd>

<dt><code>(main-worker)</code></dt>
<dd>
Returns the main worker.
</dd>

<dt><code>(worker-close <i>worker</i>)</code></dt>
<dd>
Stops receiving messages of <i>worker</i>. Pending messages will be processed. 
</dd>

<dt><code>(worker-shutdown <i>worker</i>)</code></dt>
<dd>
Stops receiving messages of <i>worker</i>, and discards pending messages.
</dd>

<dt><code>(worker-active? <i>worker</i>)</code></dt>
<dd>
Returns whether <i>worker</i> can receive a message or not. <code>(worker-active? <i>worker</i>)</code> returns <code>#f</code> after <code>(worker-close <i>worker</i>)</code> is called.


<dt><code>(current-priority)</code></dt>
<dd>
Returns the current priority of the current running code.
</dd>

<dt><code>(add-message-handler! <i>message</i> <i>proc</i> :key <i>priority</i>)</code></dt>
<dd>
Registers a message handler <i>proc</i> for <i>message</i>.

The received messages will be processed from higher priority. You can specify <i>priority</i> of this event. <i>priority</i> must be one of <code>'low</code>, <code>'default</code> or <code>'high</code>.  
</dd>

<dt><code>(delete-message-handler! <i>message</i>)</code></dt>
<dd>
Removes a message handler for <i>message</i>.
</dd>

<dt><code>(define-message <i>message</i> (<i>arg ...</i>) [:priority <i>priority</i>] <i>body ...</i>)</code></dt>
<dd>
This is a handy macro to define a message handler, same as <code>add-message-handler!</code>. 
</dd>

<dt><code>(object-apply (<i>worker</i> &lt;grv-worker&gt;) <i>message</i> <i>arg ...</i>)</code></dt>
<dd>
Sends <i>message</i> to <i>worker</i>, and returns <code>&lt;grv-promise&gt;</code> object which will hold the results of the message handler. You extract the results with <code>(await <i>promise</i>)</code>. 
</dd>
</dl>


### Asynchronous utilities

<dl>
<dt><code>(asleep <i>time</i>)</code></dt>
<dt><code>(asleep <i>sec</i>)</code></dt>
<dd>
Suspends the current running code until the time <i>time</i> is reached or the number of seconds <i>sec</i> elapses.
</dd>

<dt><code>(when-time-passed <i>sec</i> <i>body ...</i>)</code></dt>
<dd>
Executes <i>body ...</i> when the number of seconds <i>sec</i> passed after the last execution.

For example, <i>body ...</i> will run every 0.1 second in this case.
<pre>
<code>
(while #t
  (when-time-passed 0.1
    <i>body ...</i>))
</code>
</pre>
</dd>
</dl>

#### Graviton promise

<dl>
<dt><code>(await <i>promise</i>)</code></dt>
<dd>
Waits until <i>promise</i> has values, then returns the values.
</dd>

<dt><code>(disable-async-wait)</code></dt>
<dt><code>(disable-async-wait <i>bool</i>)</code></dt>
<dd>
<code>disable-async-wait</code> is a parameter to control whether <code>await</code> can yield the right of execution to other code or not. If this parameter is <code>#t</code>, <code>await</code> blocks the current thread until values are set in <code>&lt;grv-promise&gt;</code>.

The default is <code>#f</code> (can yield, don't block), and you don't need to change the parameter. However, you may change the parameter to <code>#t</code> if someone calls <code>(reset ...)</code>. Graviton's asynchronous mechanism is built on a partial continuation in worker, but another partial contitnuation can break the asynchronous mechanism (for example, implicit delimited contiations by <code>Scm_Eval</code>).   
</dd>
</dl>

#### Channel

<code>&lt;channel&gt;</code> is a queue which supports Graviton's asynchronous mechanism. You can use it for inter-worker communication.

<dl>
<dt><code>(make-channel)</code></dt>
<dd>
Makes a channel.
</dd>

<dt><code>(channel-send <i>channel</i> <i>value ...</i>)</code></dt>
<dd>
Adds <i>value ...</i> to <i>channel</i>.
</dd>

<dt><code>(channel-recv <i>channel</i> :optional <i>fallback</i>)</code></dt>
<dd>
Retrieves a value from <i>channel</i>. 
If no values exist in it, waits until a value is added or <i>fallback</i> is returned.

If <i>channel</i> is closed, returns <code>&lt;eof-object&gt;</code>.
</dd>

<dt><code>(channel-close <i>channel</i>)</code></dt>
<dd>
Closes <i>channel</i>.
</dd>

<dt><code>(channel-closed? <i>channel</i>)</code></dt>
<dd>
Returns whether <i>channel</i> is closed or not.
</dd>
</dl>


### Concurrency and Parallelism

<dl>
<dt><code>(concurrent <i>body ...</i>)</code></dt>
<dd>
Executes <i>body ...</i> in concurrent. It means the <i>body</i> will be executed in the current worker after the current code yields the execution.

This macro returns <code>&lt;grv-promise&gt;</code>, so that you can get the results of <i>body ...</i> from it.

NOTE: <i>body ...</i> may be executed in parallel if the current worker has multiple threads.
</dd>

<dt><code>(concurrent/await <i>body ...</i>)</code></dt>
<dd>
It is identical to <code>(await (concurrent <i>body ...</i>))</code>.
</dd>

<dt><code>(parallel <i>body ...</i>)</code></dt>
<dd>
Executes <i>body ...</i> in parallel. It means the <i>body</i> is executed in newly created worker.

This macro returns <code>&lt;grv-promise&gt;</code>, so that you can get the results of <i>body ...</i> from it.
</dd>

<dt><code>(parallel/await <i>body ...</i>)</code></dt>
<dd>
It is identical to <code>(await (parallel <i>body ...</i>)</code>.
</dd>
</dl>


### REPL

<dl>
<dt><code>(grv-repl :optional <i>reader</i> <i>evaluator</i> <i>printer</i> <i>prompter</i> <i>repl-group</i>)</code></dt>
<dd>
Starts read-eval-print loop. You must start this REPL in a worker.
If multiple REPLs are started, one of them is active in a REPL group. If <i>repl-group</i> is omitted, the default REPL group, which is globally defined, will be used.
</dd>

<dt><code>(make-repl-group)</code></dt>
<dd>
Makes a new REPL group.
</dd>

<dt><code>(next-repl :optional <i>repl-group</i>)</code></dt>
<dd>
Activates the next REPL in <i>repl-group</i>. If <i>repl-group</i> is omitted, the current REPL's group will be used.
</dd>

<dt><code>(list-repl :optional <i>repl-group</i>)</code></dt>
<dd>
Returns a list of REPL in <i>repl-group</i>. If <i>repl-group</i> is omitted, the current REPL's group will be used.
</dd>

<dt><code>(select-repl <i>repl</i>)</code></dt>
<dd>
Activates the specified REPL. The <i>repl</i> must be in a group of the current REPL.
</dd>
</dl>

## Module: graviton.grut

One of the motivations to develop Graviton is to quickly make an old-style computer UI (the days of CUI, i.e., text console + single graphics screen). graviton.grut, which stands for "GRaviton Utility Toolkit," provides miscellaneous utilities to support such UI. 

<dl>
<dt><code>(load-image <i>url</i> :key <i>on-error</i>)</code></dt>
<dd>
Loads an image from <i>url</i> and returns <code>&lt;html-image-element&gt;</code> object (which is <code>HTMLImageElement</code> in JavaScript).

The keyword argument <code>:on-error</code> can be a keyword <code>:error</code> (default) or <code>#f</code>. If it's the former, an error is signaled when the image can't be loaded from the URL. If it's the latter, <code>load-image</code> just returns <code>#f</code>.
</dd>

<dt><code>(load-audio <i>url</i> :key <i>on-error</i>)</code></dt>
<dd>
Loads an audio data from <i>url</i> and returns <code>&lt;html-audio-element&gt;</code> object (which is <code>HTMLAudioElement</code> in JavaScript).

The keyword argument <code>:on-error</code> can be a keyword <code>:error</code> (default) or <code>#f</code>. If it's the former, an error is signaled when the audio data can't be loaded from the URL. If it's the latter, <code>load-audio</code> just returns <code>#f</code>.
</dd>

<dt><code>(alist->style <i>alist</i>)</code></dt>
<dd>
Returns a string which is for the style attribute of HTML element. <i>alist</i> is an alist, the key is an style attribute name and the value is the attribute's value. If the value is <code>#f</code>, the style attribute will be ignored. 
</dd>

<dt id="grut-canvas-window"><code>(grut-canvas-window <i>width</i> <i>height</i> :key <i>id</i> <i>title</i> <i>background-color</i> <i>window-width</i> <i>window-height</i> <i>resizable?</i> <i>fit</i> <i>margin</i></code>
<dd>
Creates <code>&lt;grv-window&gt;</code> which has a <code>&lt;canvas&gt;</code> element. <i>width</i> and <i>height</i> are the resolution of the canvas.
  <dl>
    <dt><code>id</code></dt>
    <dd>
      The element ID of this <code>&lt;canvas&gt;</code> element. The default is "canvas".<br>
      You can specify a list of IDs to make multiple layered canvases. The first ID points to the backmost canvas, and the last ID points to the foreground canvas.
    </dd>
    <dt><code>title</code></dt>
    <dd>
      The title of this window.
    </dd>
    <dt><code>background-color</code></dt>
    <dd>
      The background color of this window.
    </dd>
    <dt><code>window-width</code></dt>
    <dd>
      The width of this window.
    </dd>
    <dt><code>window-height</code></dt>
    <dd>
      The height of this window.
    </dd>
    <dt><code>resizable?</code></dt>
    <dd>
      Whether this window is resizable or not. The default is <code>#t</code>.
    </dd>
    <dt><code>fit</code></dt>
    <dd>
      How to fit the size and position of this <code>&lt;canvas&gt;</code> element to this window. The value must be one of these values.
      <dl>
        <dt><code>'contain</code> (default)</dt>
        <dd>
          The canvas is expanded or shrank to fit the window with keeping the aspect ratio. If the canvas's aspect ratio and the window's aspect ratio are different, the canvas will be "<a href="https://en.wikipedia.org/wiki/Letterboxing_(filming)">letterboxed</a>".
        </dd>
        <dt><code>'cover</code></dt>
        <dd>
          The canvas is expanded or shrank to fit the window with keeping the aspect ratio. If the canvas's aspect ratio and the window's aspect ratio are different, the canvas will be clipped to fit. 
        </dd>
        <dt><code>'fill</code></dt>
        <dd>
          The canvas is expanded or shrank to fit the window without keeping the canvas's orignal aspect ratio. The entire canvas will completely fill the window.
        </dd>
        <dt><code>'none</code></dt>
        <dd>
          The canvas will not be resized.
        </dd>
      </dl>
    </dd>
    <dt><code>margin</code></dt>
    <dd>
      The margin of this <code>&lt;canvas&gt;</code> element.
    </dd>
  </dl>
</dd>

<dt><code>(grut-text-window :key <i>id</i> <i>title</i> <i>column</i> <i>row</i> <i>font</i> <i>font-size</i> <i>color</i> <i>background-color</i> <i>window-width</i> <i>window-height</i> <i>resizable?</i> <i>fit</i> <i>scrollbar?</i> <i>padding</i></code>
<dd>
Creates <code>&lt;grv-window&gt;</code> which has a <a href="#text-console"><code>&lt;grut-text&gt;</code></a> element.
  <dl>
    <dt><code>id</code></dt>
    <dd>
      The element ID of this <code>&lt;grut-text&gt;</code> element. The default is "text-console".<br>
      You can specify a list of IDs to make multiple layered text elements. The first ID points to the backmost text, and the last ID points to the foreground text.
    </dd>
    <dt><code>title</code></dt>
    <dd>
      The title of this window.
    </dd>
    <dt><code>column</code></dt>
    <dd>
      The number of columns of this <code>&lt;grut-text&gt;</code> element. The width of <code>&lt;grut-text&gt;</code> will be computed with this value and the font size.
    </dd>
    <dt><code>row</code></dt>
    <dd>
      The number of rows of this <code>&lt;grut-text&gt;</code> element. The height of <code>&lt;grut-text&gt;</code> will be computed with this value and the font size.
    </dd>
    <dt><code>font</code></dt>
    <dd>
      The font of this <code>&lt;grut-text&gt;</code> element.
    </dd>
    <dt><code>font-size</code></dt>
    <dd>
      The font size of this <code>&lt;grut-text&gt;</code> element.
    </dd>
    <dt><code>color</code></dt>
    <dd>
      The text color of this <code>&lt;grut-text&gt;</code> element. The default is "white".
    </dd>
    <dt><code>background-color</code></dt>
    <dd>
      The background color of this window. The default is "black".
    </dd>
    <dt><code>window-width</code></dt>
    <dd>
      The width of this window.
    </dd>
    <dt><code>window-height</code></dt>
    <dd>
      The height of this window.
    </dd>
    <dt><code>resizable?</code></dt>
    <dd>
      Whether this window is resizable or not. The default is <code>#t</code>.
    </dd>
    <dt><code>fit</code></dt>
    <dd>
      How to fit the size and position of this <code>&lt;grut-text&gt;</code> element to this window. The available values are the same as <a href="#grut-canvas-window"><code>grut-canvas-window</code></a>.
    </dd>
    <dt><code>scrollbar?</code></dt>
    <dd>
      Whether the vertical scrollbar is shown or not. The default is <code>#f</code> (the scroll bar is hidden).
      The horizontal scrollbar is always hidden.
    </dd>
    <dt><code>padding</code></dt>
    <dd>
      The padding of this this <code>&lt;grut-text&gt;</code> element.
    </dd>
  </dl>
</dd>

<dt><code>(grut-text+canvas-window width height :key  <i>text-id</i> <i>canvas-id</i> <i>title</i> <i>column</i> <i>row</i> <i>font</i> <i>font-size</i> <i>color</i> <i>background-color</i> <i>window-width</i> <i>window-height</i> <i>resizable?</i> <i>fit</i> <i>scrollbar?</i> <i>margin</i> <i>padding</i></code></dt>
<dd>
Creates <code>&lt;grv-window&gt;</code> which has a <a href="#text-console"><code>&lt;grut-text&gt;</code></a> element and a <code>&lt;canvas&gt;</code> element. <i>width</i> and <i>height</i> are the resolution of the canvas.
    <dt><code>text-id</code></dt>
    <dd>
      The element ID of this <code>&lt;grut-text&gt;</code> element. The default is "text-console".<br>
      You can specify a list of IDs to make multiple layered text elements like <code>grut-text-window</code>.
    </dd>
    <dt><code>canvas-id</code></dt>
    <dd>
      The element ID of this <code>&lt;canvas&gt;</code> element. The default is "canvas".
      You can specify a list of IDs to make multiple layered canvases like <code>grut-canvas-window</code>.
    </dd>
    <dt><code>title</code></dt>
    <dd>
      The title of this window.
    </dd>
    <dt><code>column</code></dt>
    <dd>
      The number of columns of this <code>&lt;grut-text&gt;</code> element. The width of <code>&lt;grut-text&gt;</code> will be computed with this value and the font size.
    </dd>
    <dt><code>row</code></dt>
    <dd>
      The number of rows of this <code>&lt;grut-text&gt;</code> element. The height of <code>&lt;grut-text&gt;</code> will be computed with this value and the font size.
    </dd>
    <dt><code>font</code></dt>
    <dd>
      The font of this <code>&lt;grut-text&gt;</code> element.
    </dd>
    <dt><code>font-size</code></dt>
    <dd>
      The font size of this <code>&lt;grut-text&gt;</code> element.
    </dd>
    <dt><code>color</code></dt>
    <dd>
      The text color of this <code>&lt;grut-text&gt;</code> element. The default is "white".
    </dd>
    <dt><code>background-color</code></dt>
    <dd>
      The background color of this window. The default is "black".
    </dd>
    <dt><code>window-width</code></dt>
    <dd>
      The width of this window.
    </dd>
    <dt><code>window-height</code></dt>
    <dd>
      The height of this window.
    </dd>
    <dt><code>resizable?</code></dt>
    <dd>
      Whether this window is resizable or not. The default is <code>#t</code>.
    </dd>
    <dt><code>fit</code></dt>
    <dd>
      How to fit the size and position of this <code>&lt;grut-text&gt;</code> and <code>&lt;canvas&gt;</code> element to this window. The available values are the same as <a href="#grut-canvas-window"><code>grut-canvas-window</code></a>.
    </dd>
    <dt><code>scrollbar?</code></dt>
    <dd>
      Whether the vertical scrollbar is shown or not. The default is <code>#f</code> (the scroll bar is hidden).
      The horizontal scrollbar is always hidden.
    </dd>
    <dt><code>margin</code></dt>
    <dd>
      The margin of this <code>&lt;canvas&gt;</code> element.
    </dd>
    <dt><code>padding</code></dt>
    <dd>
      The padding of this this <code>&lt;grut-text&gt;</code> element.
    </dd>
  </dl>
</dd>
</dl>


### Audio

<dl>
<dt><code>(play-mml <i>track</i> <i>mml</i> ...)</code></dt>
<dd>
Plays music that is described in <i>mml</i> (Music Macro Language). <i>track</i> is a keyword in which the music is played. <i>track</i> and <i>mml</i> is a pair, and you can specify multiple <i>track</i> and <i>mml</i> pairs (for example, <code>(play-mml :track1 '(c d e) :track2 '(e f g))</code>).

The Music Macro Language is a list of these elements.
  <dl>
    <dt><code><i>note symbol (c, d, e, f, g, a, b and qualifiers)</i></code></dt>
    <dd>
      <code>c</code>, <code>d</code>, <code>e</code>, <code>f</code>, <code>g</code>, <code>a</code> and <code>b</code> correspond to a scale. You can concatitate them to describe a chord. For example, <code>'ceg</code> means Cmajor. <code>+</code> and <code>-</code> means sharp and flat. <code>'c+</code> is "C sharp", and <code>'b-</code> means "B flat". <br><br>
      You can add a number and a dot after the note symbol to describe the length. <code>'c4</code> means a quarter note, and <code>'c8.</code> means dotted eighth note. If the length is omitted, the default length is used. <br><br>
      The notes can be concatinated with <code>&</code> for slur and tie like <code>'c&c</code> and <code>'c&d</code>.
    </dd>
    <dt><code>'r<i>length</i></code></dt>
    <dd>
      Rest. <code>'r4</code> means a quarter rest. If <i>length</i> is omitted, the default length is used. 
    </dd>
    <dt><code>'x<i>length</i></code></dt>
    <dd>
      Noise.
    </dd>
    <dt><code>:tempo <i>bpm</i></code></dt>
    <dt><code>'t<i>bpm</i></code></dt>
    <dd>
      Tempo. <i>bpm</i> can't be omitted.
    </dd>
    <dt><code>:length <i>bpm</i></code></dt>
    <dt><code>'l<i>length</i></code></dt>
    <dd>
      Changes the default length of notes.
    </dd>
    <dt><code>:octave <i>bpm</i></code></dt>
    <dt><code>'o<i>octave</i></code></dt>
    <dd>
      Changes the octave.
    </dd>
    <dt><code>'&gt;</code></dt>
    <dd>
      Increases the octave.
    </dd>
    <dt><code>'&lt;</code></dt>
    <dd>
      Decreases the octave.
    </dd>
    <dt><code>:volume <i>volume</i></code></dt>
    <dt><code>'v<i>volume</i></code></dt>
    <dd>
      Changes the volume. <i>volume</i> must be a number from 0 to 1.
    </dd>
    <dt><code>:stereo-pan <i>pan</i></code></dt>
    <dt><code>'p<i>pan</i></code></dt>
    <dd>
      Controls a pan. <i>pan</i> is a number from -1 to 1, -1 means full left pan, and 1 means full right pan. 
    </dd>
    <dt><code>:gate/step <i>ratio</i></code></dt>
    <dt><code>'q<i>ratio</i></code></dt>
    <dd>
      Changes a ratio of gate time (the sound is actually sounded) and the sound length. <i>ratio</i> is a number from 0 to 1. The default is 7/8.
    </dd>
    <dt><code>:wave-form <i>wave-type</i></code></dt>
    <dt><code>:wave-form (<i>cosine-terms</i> <i>sine-terms</i>)</code></dt>
    <dd>
      Changes the wave shape of sounds. You can specify the shape with a symbol or cosine and sine-terms of the wave.<br>
      The wave type symbol is <code>'sine</code>, <code>'square</code>, <code>'sawtooth</code> or <code>'triangle</code>.<br>
      <i>cosine-terms</i> and <i>sine-terms</i> must be <code>&lt;f32vector&gt;</code>, and their lengths must be equal.
    </dd>
    <dt><code>:adsr (<i>attack</i> <i>decay</i> <i>sustain</i> <i>release</i>)</code></dt>
    <dd>
      Specifies <a href="https://en.wikipedia.org/wiki/Envelope_(music)">the envelope in ADSR</a>.
      <i>attack</i>, <i>decay</i> and <i>release</i> are time in second. <i>sustain</i> is a volume level from 0 to 1.
    </dd>
    <dt><code>:detune <i>cents</i></code></dt>
    <dd>
      Specifies detuning in cents.
    </dd>
    <dt><code>(beep <i>frequency</i> [<i>duration</i>])</code></dt>
    <dd>
      Generates a sound of <i>frequency</i> and <i>duration</i> in second. If <i>duration</i> is omitted, the default length by <code>:length</code> is used.
    </dd>
    <dt><code>(sound <i>audio-buffer</i> [<i>start</i> <i>end</i> <i>duration</i>])</code></dt>
    <dd>
      Generates a sound <i>audio-buffer</i>. If <i>start</i> is specified, the sound loops from <i>start</i> (in second) to <i>end</i> (the default <i>end</i> is the end of the sound). <i>duration</i> (in second) specifies the length of the looped sound.
    </dd>
  </dl>
</dd>
<dt><code>(compile-mml <i>mml</i>)</code></dt>
<dd>
Compiles <i>mml</i>. You can pass the returned value to <code>play-music</code> to play it. 
</dd>
<dt><code>(play-music <i>track</i> <i>music</i> <i>...</i>)</code></dt>
<dd>
Plays the compiled <i>music</i> object.
</dd>

<dt><code>(resume-track <i>track</i> ...)</code></dt>
<dd>
Resumes <i>track</i>, which is paused.
</dd>
<dt><code>(resume-all-tracks)</code></dt>
<dd>
Resumes all tracks.
</dd>
<dt><code>(pause-track <i>track</i> ...)</code></dt>
<dd>
Pauses <i>track</i>.
</dd>
<dt><code>(pause-all-tracks)</code></dt>
<dd>
Pauses all tracks.
</dd>
<dt><code>(stop-track <i>track</i> ...)</code></dt>
<dd>
Stops playing the specified <i>track</i>.
</dd>
<dt><code>(stop-all-tracks)</code></dt>
<dd>
Stops playing all tracks.
</dd>
<dt><code>(wait-track <i>track</i> ...)</code></dt>
<dd>
Waits until the playing finishes in the specified <i>track</i>.
</dd>
<dt><code>(wait-all-tracks)</code></dt>
<dd>
Waits until the playing finishes in all tracks.
</dd>
<dt><code>(play-beep <i>frequency</i> <i>length</i> :key <i>wave-form</i> <i>volume</i>)</code></dt>
<dd>
Plays a sound of <i>frequency</i> and <i>length</i>. <i>length</i> is in second.
The default <i>wave-form</i> is <code>'sine</code>.
</dd>
<dt><code>(play-sound <i>url</i> :key <i>start</i> <i>end</i> <i>duration</i> <i>volume</i>)</code></dt>
<dt><code>(play-sound <i>audio-buffer</i> :key <i>start</i> <i>end</i> <i>duration</i> <i>volume</i>)</code></dt>
<dd>
Plays a sound of the specified <i>audio-buffer</i> or the downloaded data from <i>url</i>. You can loop the sound from <i>start</i> to <i>end</i> in <i>duration</i>.
</dd>
<dt><code>(load-audio-buffer <i>url</i> :key <i>on-error</i>)</code></dt>
<dd>
Loads audio data from <i>url</i>, and returns <i>audio-buffer</i> object. You can play it with <code>play-sound</code> or the MML command <code>(sound <i>audio-buffer</i> <i>...</i>)</code>.

The keyword argument <code>:on-error</code> can be a keyword <code>:error</code> (default) or <code>#f</code>. If it's the former, an error is signaled when the audio data can't be loaded from the URL. If it's the latter, <code>load-audio-buffer</code> just returns <code>#f</code>.
</dd>
</dl>

### Clipboard

<dl>
<dt><code>(copy-text-to-clipboard <i>text</i>)</code></dt>
<dd>
Copies <i>text</i> into the clipboard.
</dd>
</dl>


### Speech

`<speech-synthesis-voice>` represents a voice for the speech synthesis (it is identical to [`SpeechSynthesisVoice`](https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisVoice) in JavaScript). The class has these slots.
<dl>
<dt><code>default</code> (boolean)</dt>
<dd>
  Whether the voice is the defalut voice or not.
</dd>
<dt><code>lang</code> (string)</dt>
<dd>
  The BCP 47 language tag of this voice.
</dd>
<dt><code>local-service</code> (boolean)</dt>
<dd>
  Whether the voice is supplied by a local speech synthesizer service or not.
</dd>
<dt><code>name</code> (string)</dt>
<dd>
  The human-readable name of this voice.
</dd>
<dt><code>voice-uri</code> (string)</dt>
<dd>
  The type of URI and location of the speech synthesis service for this voice.
</dd>
</dl>

These functions are provided for Speech Synthesis.

<dl>
<dt id="query-all-voices"><code>(query-all-voices :key <i>lang</i> <i>name</i> <i>default</i> <i>local-service</i> <i>voice-uri</i> <i>wait?</i>)</code></dt>
<dd>
Returns a list of voices that matches the specified conditions. The conditions can be specified with <code><i>lang</i> <i>name</i> <i>default</i> <i>local-service</i> <i>voice-uri</i></code>. They are associated with the slots of <code>&lt;speech-synthesis-voice&gt;</code>. 

If the slot is boolean, the associated keyword parameter can takes a boolean value. The voices that matches the keyword parameter value will be returned. 

If the slot is string, the associated keyword parameter can take a string or a regular expression. The voices whose parameter contains the string or matches the regular expression will be returned.

<dl>
  <dt><code><i>wait?</i></code> (default is <code>#t</code>)</dt>
  <dd>
    Waits for the background voice loading. Some browsers loads voices in background, so <code>SpeechSynthesis.getVoices()</code> in JavaScript may not return any voices until the load is completed. If <code><i>wait?</i></code> is <code>#t</code>, this function waits until the getVoices() method returns voices. If no voices are available, <code>query-all-voices</code> will not be returned.   
  </dd>
</dl>
</dd>

<dt><code>(query-voice :key <i>lang</i> <i>name</i> <i>default</i> <i>local-service</i> <i>voice-uri</i> <i>wait?</i>)</code></dt>
<dd>
Returns one of the voices that matches the specified conditions. The keyword parameters are the same as <a href="#query-all-voices"><code>query-all-voices</code></a>.
</dd>

<dt><code>(speak <i>text</i> :key <i>voice</i>)</code></dt>
<dd>
Speaks <i>text</i> with <i>voice</i>. If <i>voice</i> is omitted, the default voice will be used.
</dd>

<dt><code>(pause-speech)</code></dt>
<dd>
Pauses the current speech.
</dd>

<dt><code>(resume-speech)</code></dt>
<dd>
Resumes the paused speech.
</dd>

<dt><code>(cancel-speech)</code></dt>
<dd>
Cancels the current speech.
</dd>
</dl>


### Text console

graviton.grut module provides `<grut-text>` element that represents a text console. You can input a text using line edit and output a text. You can also control the text console using ANSI escape sequence.

`<grut-text>` is a `<jsobject>`, and it also inherits `<virtual-output-port>`. You can write a text to it, so that the text will be displayed in the text console element (for example, `(format text-console "Hello, world")`).

`<grut-text>` can accept generic functions of [`text.console`](http://practical-scheme.net/gauche/man/?p=text.console).

<dl>
<dt><code>html:grut-text</code></dt>
<dd>
Constructs a <code>&lt;grut-text&gt;</code> element. You can use it as a part of HTML document tree by <a href="http://practical-scheme.net/gauche/man/?p=text.html-lite"><code>text.html-lite</code> module</a>.
</dd>

<dt><code>(set-line-style! <i>text-console</i> <i>row</i> <i>style</i> <i>value</i>)</code></dt>
<dd>
Sets the HTML <i>style</i> of the line <i>row</i> to <i>value</i>. 
</dd> 

<dt><code>(scroll-up <i>text-console</i> :optional <i>n</i>)</code></dt>
<dd>
Scrolls up <i>text-console</i> by <i>n</i> lines. The default <i>n</i> is 1.
</dd>

<dt><code>(scroll-down <i>text-console</i> :optional <i>n</i>)</code></dt>
<dd>
Scrolls down <i>text-console</i> by <i>n</i> lines. The default <i>n</i> is 1.
</dd>

<dt><code>(scroll-to <i>text-console</i> <i>row</i> :optional <i>align-to-top</i>)</code></dt>
<dd>
Scrolls to the line <i>row</i>. If <i>align-to-top</i> is <code>#t</code>, the line <i>row</i> will be aligned at the top. Otherwise, the line will be aligned at the bottom. The default value of <i>align-to-top</i> is <code>#t</code>.
</dd>

<dt><code>(compute-character-position&size <i>text-console</i> <i>column</i> <i>row</i>)</code></dt>
<dd>
Returns the rectangle in the viewport which contains the character at (<i>column</i>, <i>row</i>). This function returns 4 values (x, y, width and height).
</dd>

<dt><code>(get-input-text <i>text-console</i> :optional <i>wait?</i>)</code></dt>
<dd>
Returns a string in the input queue of <i>text-console</i>. 
If the input queue is empty and <i>wait?</i> is <code>#f</code> (default), <code>#f</code> is returned.

If <i>wait?</i> is <code>#t</code>, this function waits for an input event if the queue is empty. 
</dd>
</dl>

#### Line Edit

<dl>
<dt><code>(make-keymap :optional <i>parent</i>)</code></dt>
<dd>
Makes a keymap. If a <i>parent</i> keymap is specified, the newly created keymap inherits the <i>parent</i>. 
</dd>
<dt><code>(global-keymap)</code></dt>
<dd>
Returns the global keymap, which is effective at first.
</dd>
<dt><code>(bind-key <i>keymap</i> <i>key</i> <i>action</i> :key <i>use-clipboard-text?</i>)</code></dt>
<dd>
Assigns <i>action</i> to <i>key</i> in <i>keymap</i>.

<i>key</i> is <a href="https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values">a key value of KeyboardEvent</a>. If you want to specify modifier keys, Alt, Meta, Ctrl and Shift, you need to add prefixes "A-", "M-", "C-" and "S-" in this order. For example, if you want to specify "Ctrl+Shift+PageDown", the key is "C-S-PageDown".

You can also specify continuous keys like "Ctrl+X Ctrl+S" in Emacs. In this case, the <i>key</i> is "C-x C-s". 

<i>action</i> must be a string or <code>&lt;procedure&gt;</code>. If <i>action</i> is a string, the string will be inserted if the <i>key</i> is pressed. If <i>action</i> is a procedure, the procedure will be called with <code>&lt;input-context&gt;</code>. You can get information of line editing from the context.

If you want to use a clipboard text, you need to specify <code>#t</code> to <code>:use-clipboard-text?</code> keyword parameter. You can get the clipboard text from the <code>&lt;input-context&gt;</code>.
</dd>

<dt><code>(switch-keymap <i>input-context</i> <i>keymap</i>)</code></dt>
<dd>
Switches the current keymap to <i>keymap</i>.
</dd> 

<dt><code>(read-text/edit <i>text-console</i> :key <i>prompt</i> <i>keymap</i> <i>input-continues</i> <i>initial-text</i> <i>cursor-column</i> <i>cursor-row</i> <i>on-change</i> <i>on-input</i> <i>data-alist</i>)</code></dt>
<dd>
Reads a text that the user inputs on <i>text-console</i>. The user can use line edit in the input.
  <dl>
    <dt><code>prompt</code></dt>
    <dd>
      The prompt string of this input. The default is an empty string.
    </dd>
    <dt><code>keymap</code></dt>
    <dd>
      The keymap used in this input.
    </dd>
    <dt><code>input-continues</code></dt>
    <dd>
      The procedure that determines whether the input continues or finishes. This procedure is called with input-context  when Enter is pressed. If it returns <code>#f</code>, the input finishes and <code>read-text/edit</code> returns the input text. Otherwise, the input continues.<br>
      If <i>input-continues</i> is <code>#f</code> (default), the input finishes when Enter is pressed. If you don't need multiple line edit, you don't need to specify <i>input-continues</i>.
    </dd>
    <dt><code>initial-text</code></dt>
    <dd>
      The ininitial text of this input.
    </dd>
    <dt><code>cursor-column</code></dt>
    <dd>
      The column number of the initial cursor position.
    </dd>
    <dt><code>cursor-column</code></dt>
    <dd>
      The row number of the initial cursor position.
    </dd>
    <dt><code>on-change</code></dt>
    <dd>
      The procedure that is called with <code>&lt;input-context&gt;</code> when the input text is changed.
    </dd>
    <dt><code>on-input</code></dt>
    <dd>
      The procedure that is called with <code>&lt;input-context&gt;</code> when the user types any keys.
    </dd>
    <dt><code>data-alist</code></dt>
    <dd>
      The data that will be passed to <code>&lt;input-context&gt;</code>. You can get the data with <code>input-context-data-get</code> and modify it with <code>input-context-data-put!</code>.
    </dd>
  </dl>
</dd>

<dt><code>(clipboard-text <i>input-context</i>)</code></dt>
<dd>
Returns the text in the clipboard. If no text is available, <code>#f</code> is returned.
</dd>

<dt><code>(input-context-text-line <i>input-context</i> :optional <i>row</i>)</code></dt>
<dd>
Returns the line of the input text at line <i>row</i>. The default <i>row</i> is the current line. 
</dd>

<dt><code>(input-context-text-content <i>input-context</i>)</code></dt>
<dd>
Returns the whole text of the input text.
</dd>

<dt><code>(input-context-data-get <i>input-context</i> <i>key</i> :optional <i>default</i>)</code></dt>
<dd>
Searches <i>key</i> in <i>input-context</i>, and returns its value if found. Otherwise, returns <i>default</i> (the default is <code>#f</code>).
</dd>

<dt><code>(input-context-data-put! <i>input-context</i> <i>key</i> <i>value</i>)</code></dt>
<dd>
Puts <i>key</i> with <i>value</i> in <i>input-context</i>.
</dd>

<dt><code>(input-context-data-delete! <i>input-context</i> <i>key</i>)</code></dt>
<dd>
Deletes <i>key</i> in <i>input-context</i>.
</dd>

<dt><code>(edit:backward-delete-char <i>input-context</i>)</code></dt>
<dd>
Deletes the previous character.
</dd>

<dt><code>(edit:beginning-of-edit-area <i>input-context</i>)</code></dt>
<dd>
Moves the cursor to the beginning of the input area.
</dd>

<dt><code>(edit:beginning-of-line <i>input-context</i>)</code></dt>
<dd>
Moves the cursor to the beginning of the current line.
</dd>

<dt><code>(edit:cancel-edit <i>input-context</i>)</code></dt>
<dd>
Cancels the input text. <code>read-text/edit</code> returns <code>#f</code>.
</dd>

<dt><code>(edit:copy <i>input-context</i>)</code></dt>
<dd>
Copies the selected text into the clipboard.
</dd>

<dt><code>(edit:cut <i>input-context</i>)</code></dt>
<dd>
Copies the selected text into the clipboard, and deletes the text.
</dd>

<dt><code>(edit:delete-char <i>input-context</i>)</code></dt>
<dd>
Deletes the current character.
</dd>

<dt><code>(edit:end-of-edit-area <i>input-context</i>)</code></dt>
<dd>
Moves the cursor to the end of the input area.
</dd>

<dt><code>(edit:end-of-line <i>input-context</i>)</code></dt>
<dd>
Moves the cursor to the end of the current line.
</dd>

<dt><code>(edit:forward-char <i>input-context</i>)</code></dt>
<dd>
Moves the cursor one character forward.
</dd>

<dt><code>(edit:insert-string <i>input-context</i> <i>string</i>)</code></dt>
<dd>
Inserts <i>string</i> at the current cursor position.
</dd>

<dt><code>(edit:newline-or-commit <i>input-context</i>)</code></dt>
<dd>
Finishes the input, and returns the input text if <code>input-continues</code> that is specified by <code>read-text/edit</code> returns <code>#f</code>. Otherwise, inserts a newline. 
</dd>

<dt><code>(edit:next-line <i>input-context</i>)</code></dt>
<dd>
Moves the cursor vertically down one line.
</dd>

<dt><code>(edit:page-up <i>input-context</i>)</code></dt>
<dd>
Moves the cursor vertically up one page.
</dd>

<dt><code>(edit:page-down <i>input-context</i>)</code></dt>
<dd>
Moves the cursor vertically down one page.
</dd>

<dt><code>(edit:paste <i>input-context</i>)</code></dt>
<dd>
Pastes a text in the clipboard.
</dd>

<dt><code>(edit:previous-char <i>input-context</i>)</code></dt>
<dd>
Moves the cursor one character backward.
</dd>

<dt><code>(edit:previous-line <i>input-context</i>)</code></dt>
<dd>
Moves the cursor vertically up one line.
</dd>

<dt><code>(edit:select-all <i>input-context</i>)</code></dt>
<dd>
Selects all text in the current input area.
</dd>
</dl>


## JavaScript FFI and JSiSE (JavaScript in S-expression)

Graviton provides S-expression JavaScript (JSiSE) syntax like CiSE of Gauche. You can use it in the macros described below.

Basically, `(func arg1 arg2 ...)` in JSiSE is translated to `func(arg1, arg2, ...)` in JavaScript. A symbol in JSiSE is an identifier or a variable name in JavaScript. A dot "." in a JSiSE symbol has a special meaning, which means property reference of an object. It is the same as JavaScript. So `(console.log "Hello, world")` is `console.log("Hello, world")`, and `(set! foo.bar 1)` is `foo.bar=1`.

<dl>
<dt><code>(define-jsvar <i>var</i> <i>[value]</i>)</code></dt>
<dd>
Defines a JavaScript variable <i>var</i> in the current JavaScript module. This JavaScript module is created implicitly per Gauche module. This variable <i>var</i> can't be seen from other modules. If <i>value</i> is omitted, the variable is initialized with <code>undefined</code>. 
</dd>

<dt><code>(define-jsfn (<i>func</i> <i>args ...</i>) <i>body ...</i>)</code></dt>
<dd>
Defines a JavaScript function <i>func</i> in the current JavaScript module. 
</dd>

<dt><code>(inline-js <i>body ...</i>)</code></dt>
<dd>
Embeds JavaScript code in the toplevel of the current JavaScript module. This code runs when the module is loaded.
</dd>

<dt><code>(import-js <i>module-path</i> :as <i>name</i>)</code></dt>
<dt><code>(import-js <i>module-path</i> [:only (<i>only-export ...</i>)] [:rename ((<i>rename-export</i> <i>alias</i>) <i>...</i>)])</code></dt>
<dd>
Imports the JavaScript module from <i>module-path</i> into the current module.

The former statement is translated to <code>import * as <i>name</i> from <i>module-path</i></code>, it will import all the contents of the module as <i>name</i>.

The latter statement is translated to <code>import {<i>only-export</i>, <i>rename-export</i> as <i>alias</i>, <i>...</i>} from <i>module-path</i></code>, it will import the selected contents of the module. In the latter statement, you need to specify at least one of <code>:only</code> or <code>:rename</code> keyword.
</dd>

<dt><code>(jslet ((<i>jsvar</i> <i>[value]</i>) <i>...</i>) <i>body ...</i>)</code></dt>
<dd>
Embeds JavaScript code. <i>value</i> is an object in Scheme world. It is transferred to JavaScript world and bound to <i>jsvar</i>. <code>jslet</code> doesn't return a meaningful value. Currently, it returns <code>#&lt;undef&gt;</code>.

If <i>value</i> is omitted, an object which is referenced by the same name <i>jsvar</i> in Scheme world, is bound.

In run-time, <code>jslet</code> sends a request to the client (browser or graviton-player) to execute <i>body ...</i>. But the request is buffered until the current continuation finishes. If you want to send the request immediately, call <code>(asleep 0)</code>.
</dd>

<dt><code>(jslet/async ((<i>jsvar</i> <i>[value]</i>) <i>...</i>) <i>body ...</i>)</code></dt>
<dt><code>(jslet/await ((<i>jsvar</i> <i>[value]</i>) <i>...</i>) <i>body ...</i>)</code></dt>
<dd>
Basically, <code>jslet/async</code> and <code>jslet/await</code> are the same as <code>jslet</code>, but they can return values. You can use <code>(respond <i>val ...</i>)</code> macro to return values to Scheme world.

<code>jslet/async</code> returns <code>&lt;grv-promise&gt;</code> object immediately, and you need to use <code>(await <i>promise</i>)</code> to get the values. 

<code>jslet/await</code> is identical to <code>(await (jslet/async <i>...</i>))</code>.

<code>await</code> doesn't block the current thread. The continuation is called after the JavaScript code returns values. Until then, the thread can execute other code (event handler or other waiting continuations, etc.).

Like <code>jslet</code>, the request to execute <i>body ...</i> is buffered. Calling <code>await</code> will send the buffered requests because it finishes the current continuation. <code>jslet/await</code> always calls <code>await</code>, so the buffered requests are sent at <code>jslet/await</code> location. 
</dd>
</dl>


The following sections explain the syntax of JSiSE.

### Literals

- `#t` means `true`.
- `#f` means `false`.
- `null` means `null`.
- `undefined` or `#<undef>` means `undefined`.
- `#(v0 v1 v2 ...)` means a vector `[v0, v1, v2, ...]`.
- `"string"` means a string.


### Vector

<dl>
<dt><code>(vector <i>val ...</i>)</code></dt>
<dd>It means a vector <code>[<i>val</i>, <i>...</i>]</code>.</dd>

<dt><code>(vector-ref <i>vec</i> <i>i</i>)</code></dt>
<dd>It means <code><i>vec</i>[<i>i</i>]</code>.</dd>

<dt><code>(vector-set! <i>vec</i> <i>i</i> <i>val</i>)</code></dt>
<dd>It means <code><i>vec</i>[<i>i</i>]=<i>val</i></code>.</dd>
</dl>


### Object

<dl>
<dt><code>(object (<i>key</i> . <i>val</i>) <i>...</i>)</code></dt>
<dd>It means a JavaScript object <code>{<i>key</i>: <i>val</i>, <i>...</i>}</code>. <i>key</i> must be a string.</dd>

<dt><code>(new <i>class</i> <i>args ...</i>)</code></dt>
<dt><code>(make <i>class</i> <i>args ...</i>)</code></dt>
<dd>It means <code>new <i>class</i>(<i>args, ...</i>)</code>.

<dt><code>(~ <i>expr</i> <i>attr ...</i>)</code></dt>
<dt><code>(ref <i>expr</i> <i>attr ...</i>)</code></dt>
<dd>It means a property reference. If the attribute <i>attr</i> is a symbol, it is translated to <code>(<i>expr</i>).<i>attr</i></code>. Otherwise, it is translated to <code>(<i>expr</i>)[<i>attr</i>]</code>. You can pass multiple <i>attr</i> for reference chain.</dd>
</dl>


### Control flow

<dl>
<dt><code>(if <i>expr</i> <i>then-body</i> <i>[else-body]</i>)</code></dt>
<dd>It means <code>if (<i>expr</i>) { <i>then-body</i> } else { <i>else-body</i> }</code>. This <code>if</code> macro is available in an expression. In that case, the <code>if</code> is tralslated to <code>(<i>expr</i>) ? (<i>then-body</i>) : (<i>else-body</i>)</code>.</dd>

<dt><code>(cond (<i>expr</i> <i>then ...</i>) ... [(else <i>else-body ...</i>)])</code></dt>
<dd>It is translated to continuous if-else statements. This <code>cond</code> macro is available in an expression like <code>if</code> macro.</dd>

<dt><code>(dotimes (<i>var</i> <i>num-expr</i>) <i>body ...</i>)</code></dt>
<dd>It repeats <i>body ...</i> for a number of times <i>num-expr</i>. <i>var</i> indicates the number of the loop (starts from 0).</dd>

<dt><code>(when <i>expr</i> <i>body ...</i>)</code></dt>
<dd>It means <code>if (<i>expr</i>) { <i>body...</i> }</code>.</dd>

<dt><code>(unless <i>expr</i> <i>body ...</i>)</code></dt>
<dd>It means <code>if (!<i>expr</i>) { <i>body ...</i>}</code>.</dd>

<dt><code>(case <i>key</i> ((<i>data0</i> <i>data1 ...</i>) <i>body ...</i>) [(else <i>else-body ...</i>)])</code></dt>
<dd>It means <code>switch (<i>key</i>) { case <i>data0</i>: case <i>data1</i>: ... { <i>body ...</i> } break; default: <i>else-body ...</i>}</code>.</dd>
</dl>

<dl>
<dt><code>(dotimes (var num-expr) body ...)</code></dt>
<dd> It is equivalent to Scheme's <code>dotimes</code>. It repeats <i>body</i> for a number of times.</dd>

<dt><code>(while <i>expr</i> <i>body ...</i>)</code></dt>
<dd>It means <code>while (<i>expr</i>){<i>body ...</i>}</code>.</dd>

<dt><code>(for-each <i>proc</i> <i>coll</i>)</code><dt>
<dd>It means <code><i>coll</i>.forEach(<i>proc</i>)</code>.</dd>

<dt><code>(return <i>[val]</i>)</code></dt>
<dd>It means <code>return <i>[val]</i></code>.</dd>

<dt><code>(begin <i>body ...</i>)</code></dt>
<dd>It means <code>{ <i>body ...</i> }</code>.</dd>
</dl>


### Local variables definition

<dl>
<dt><code>(let ((<i>var</i> <i>expr</i>) <i>...</i>) <i>body ...</i>)</code></dt>
<dt><code>(let* ((<i>var</i> <i>expr</i>) <i>...</i>) <i>body ...</i>)</code></dt>
<dd>
<code>let</code> and <code>let*</code> are equivalent to Scheme's <code>let</code> and <code>let*</code>. They creates a local scope where <i>var ...</i> are bound to the value of <i>expr ...</i>, then evaluates <i>body ...</i>.
</dd>

<dt><code>(let1 <i>var</i> <i>expr</i> <i>body ...</i>)</code></dt>
<dd>
<code>let1</code> is a convenient macro for only one variable. It is the same as <code>(let ((<i>var</i> <i>expr</i>)) <i>body ...</i>)</code>.
</dd>

<dt><code>(rlet1 <i>var</i> <i>expr</i> <i>body ...</i>)</code></dt>
<dd>
<code>rlet1</code> binds the value of <i>expr</i> to <i>var</i>, then evaluates <i>body ...</i>. After that, it returns <i>var</i>. 
</dd>
</dl>


### Assignment

<dl>
<dt><code>(set! <i>var</i> <i>expr</i>)</code></dt>
<dd>
It means <code><i>var</i>=<i>expr</i></code>.
</dd>

<dt><code>(inc! <i>var</i> <i>[delta]</i>)</code></dt>
<dd>
It means <code><i>var</i>+=<i>delta</i></code>. The default value of <i>delta</i> is 1.
</dd>

<dt><code>(dec! <i>var</i> <i>[delta]</i>)</code></dt>
<dd>
It means <code><i>var</i>-=<i>delta</i></code>. The default value of <i>delta</i> is 1.
</dd>
</dl>


### Expression operators

- `(not v)` means `!v`.
- `(or v0 v1 ...)` means `v0 || v1 || ...`.
- `(and v0 v1 ...)` means `v0 && v1 && ...`.

- `(lognot v)` means `~v`.
- `(logior v0 v1 ...)` means `v0 | v1 | ...`.
- `(logand v0 v1 ...)` means `v0 & v1 & ...`.
- `(logxor v0 v1 ...)` means `v0 ^ v1 ^ ...`.

- `(equal? v0 v1 ...)` means `v0 === v1 === ...`.
- `(< v0 v1 ...)` means `v0 < v1 < ...`.
- `(<= v0 v1 ...)` means `v0 <= v1 <= ...`.
- `(> v0 v1 ...)` means `v0 > v1 > ...`.
- `(>= v0 v1 ...)` means `v0 >= v1 >= ...`.

- `(+ v0 v1 ...)` means `v0 + v1 + ...`.
- `(- v)` means `-v`.
- `(- v0 v1 ...)` means `v0 - v1 - ...`.
- `(* v0 v1 ...)` means `v0 * v1 * ...`.
- `(/ v0 v1 ...)` means `v0 / v1 / ...`.
- `(modulo v0 v1)` means `v0 % v1`.
- `(ash n count)` means `<<` (if count is positive or zero) or `>>` (if count is negative).

- `(pre++ var)` means `++var`.
- `(pre-- var)` means `--var`.
- `(post++ var)` means `var++`.
- `(post-- var)` means `var--`.

- `(lambda (arg ...) body ...)` means `function (arg, ...) { body ... }`.

- `(is-a? expr class)` means `expr instanceof class`.


### Returning values to Scheme

<dl>
<dt><code>(respond <i>val ...</i>)</code></dt>
<dd>
It returns the values <i>val ...</i> to Scheme world.
</dd>
</dl>


### Global variable definition

<dl>
<dt><code>(define <i>var</i> <i>val</i>)</code></dt>
<dt><code>(define (<i>func</i> <i>args ...</i>) <i>body ...</i>)</code></dt>
<dd>
They are translated to <code>let <i>var</i> = <i>val</i></code> and <code>let <i>func</i> = function(<i>args, ...</i>) { <i>body ...</i> }</code>.
</dd>

<dt><code>(import <i>var</i> <i>name</i>)</code></dt>
<dt><code>(import (<i>var</i> ...) <i>name</i>)</code></dt>
<dd>
They translated to <code>const {<i>var, ...</i>} = require(<i>name</i>)</code>.
</dd>
</dl>

NOTE: `define` and `import` are intended to define a global object in `inline-js`.
