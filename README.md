# Graviton

## Table of contents

## Overview

Graviton is a library to provide Web-based UI for a standalone Gauche program. You can make UI with HTML, CSS, and JavaScript and integrate the UI into your program.

Graviton also provides JavaScript FFI with S-expression syntax so that you can embed JavaScript code naturally in your program.

You need a modern web browser to access Graviton's Web-based UI. However, Graviton provides an Electron-based client (graviton-player) to interact with the UI. Your program can act as a native GUI application using graviton-player.

CAVEAT: Graviton is a kind of "Web application framework." However, it is designed for one or a few clients. Graviton has no particular restrictions on the number of clients, but it may not be efficient when many clients use it.

## Installation

You need the latest version of Gauche and Gauche-makiki.
To build graviton-player, you also need the latest node and npm.

```
$ ./configure
$ make
$ make install
$ make install-player  # if you want to install graviton-player.
```

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
    <dt><code>client</code>
    <dd>
      Client type. The default is <code>'player</code> if graviton-player is installed. Otherwise, <code>'browser</code>.
      <dl>
        <dt><code>'browser</code></dt>
        <dd>
          Web browser. The program waits for the client requests, and can accept requests from multiple clients.
        </dd>
        <dt><code>'player</code></dt>
        <dd>
          graviton-player. The program opens a main window with graviton-player, and it exits if the window is closed.
        </dd>
        <dt><code>#f</code></dt>
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
</dl>

### Window

When Graviton receives a request from the client, it creates a new window instance and assigns a thread (called "worker") to the window instance. The code running in the worker can operate the associated window only.

<dl>
<dt><code>(grv-window <i>:key title css js head body width height resizable?</i>)</code></dt>
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
Closes the current window.
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

<dt><code>(window-parameter-atomic-ref <i>window-parameter</i> <i>proc</i>)</code></dt>
<dd>
</dd>
Calls <i>proc</i> with the current values in <i>window-parameter</i>, while locking <i>window-parameter</i>.
</dd>

<dt><code>(window-parameter-atomic-update! <i>window-parameter</i> <i>proc</i>)</code></dt>
<dd>
Calls <i>proc</i> with the current values in <i>window-parameter</i> while locking <i>window-parameter</i>, and updates the values in <i>window-parameter</i> by the returned values from <i>proc</i>.
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


## Module: graviton.grut

One of the motivations to develop Graviton is to quickly make an old-style computer UI (the days of CUI, i.e., text console + single graphics screen).

graviton.grut, which stands for "GRaviton Utility Toolkit," provides miscellaneous utilities to support such UI. Currently, it contains these utilities.
 - Simple window generation of text console + graphics
 - Text Console
 - Clipboard
 - Old-style PSG Music with MML
 - Speech Synthesis

load-image
load-audio

alist->style

grut-canvas-window
grut-text-window
grut-text+canvas-window

### grut audio

play-mml
resume-track
resume-all-tracks
pause-track
pause-all-tracks
stop-track
stop-all-tracks
wait-track
wait-all-tracks

play-beep

### grut clipboard

copy-text-to-clipboard

### grut speech

<speech-synthesis-voice>
query-all-voices
query-voice
speak
pause-speech
resume-speech
cancel-speech

### grut text

html:grut-text
clipboard-text

<input-context>
<grut-text>

get-input-text

call-with-console
putch
putstr
beep
getch
chready?
query-cursor-position
move-cursor-to
clear-screen
clear-to-eol
clear-to-eos
hide-cursor
show-cursor
cursor-down/scroll-up
cursor-up/scroll-down
query-screen-size
set-character-attribute
reset-character-attribute
with-character-attribute

set-line-style!
scroll-up
scroll-down
scroll-to

make-keymap
global-keymap
bind-key
switch-keymap

compute-character-position&size

input-context-data-get
input-context-data-put!
input-context-data-delete!
input-context-text-line
input-context-text-content

edit:backward-delete-char
edit:beginning-of-edit-area
edit:beginning-of-line
edit:cancel-edit
edit:copy
edit:cut
edit:delete-char
edit:end-of-edit-area
edit:end-of-line
edit:forward-char
edit:insert-string
edit:newline-or-commit
edit:next-line
edit:page-up
edit:page-down
edit:paste
edit:previous-char
edit:previous-line
edit:select-all
read-text/edit


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
