'use strict';

let initializeFunctions = [];

function registerInitializer(func) {
    initializeFunctions.push(func);
}

let webSocket;

function connectServer() {
    webSocket = new WebSocket('ws://' + location.host + '/s');
    webSocket.binaryType = 'arraybuffer';
    webSocket.onopen = () => {
        console.log('opened');
        startApplication();
    };
    webSocket.onclose = () => {
        console.log('closed');
        webSocket = null;
        window.close();
    };
    webSocket.onmessage = (event) => {
        try {
            if (typeof event.data === 'string') {
                let params =JSON.parse(event.data);
                dispatchJsonMessage(params);
            } else {
                dispatchBinaryMessage(event.data);
            }
        } catch (e) {
            notifyException(false, e.toString());
        }
    };
}

registerInitializer(connectServer);

let jsonTable = {};
let proxyObjectTable = {};
let enumTable = {};

function registerJson(index, json) {
    jsonTable[index] = json;
}

function fetchJson(index) {
    let json = jsonTable[index];
    jsonTable[index] = null;
    return json;
}

function linkProxyObject(index, obj) {
    proxyObjectTable[index] = obj;
}

function unlinkProxyObject(index) {
    proxyObjectTable[index] = null;
}

function getProxyObject(index) {
    return proxyObjectTable[index];
}

class DataStream {
    constructor(buf) {
        this.dataView = new DataView(buf);
        this.position = 0;
    }

    hasData() {
        return this.position < this.dataView.byteLength;
    }

    getUint() {
        let n = 0;
        let v = 0;
        let i = 0;
        do {
            v = this.dataView.getUint8(this.position);
            this.position++;;
            n += (v & 0x7f) << (i * 7);
            i++;
        } while (v > 0x7f);
        return n;
    }

    getInt() {
        let n = this.getUint();
        if ((n & 1) === 0) {
            return n >> 1;
        } else {
            return -(n >> 1) - 1;
        }
    }

    getFloat32() {
        let v = this.dataView.getFloat32(this.position, true);
        this.position += 4;
        return v;
    }

    getFloat64() {
        let v = this.dataView.getFloat64(this.position, true);
        this.position += 8;
        return v;
    }

    getInt16() {
        let v = this.dataView.getInt16(this.position, true);
        this.position += 2;
        return v;
    }

    getInt32() {
        let v = this.dataView.getInt32(this.position, true);
        this.position += 4;
        return v;
    }

    getInt8() {
        let v = this.dataView.getInt8(this.position);
        this.position += 1;
        return v;
    }

    getUint16() {
        let v = this.dataView.getUint16(this.position, true);
        this.position += 2;
        return v;
    }

    getUint32() {
        let v = this.dataView.getUint32(this.position, true);
        this.position += 4;
        return v;
    }

    getUint8() {
        let v = this.dataView.getUint8(this.position);
        this.position += 1;
        return v;
    }

    getBoolean() {
        let v = this.getUint8();
        return v !== 0;
    }

    getJson() {
        let index = this.dataView.getUint32(this.position, true);
        this.position += 4;
        return fetchJson(index);
    }

    getUint8Array() {
        let len = this.getUint32();
        let data = new Uint8Array(this.dataView.buffer, this.position, len);
        this.position += len;
        return data;
    }

    getFloat64Array() {
        let len = this.getUint32();
        let data = new Float64Array(len);
        for (let i = 0; i < len; ++i) {
            data[i] = this.getFloat64();
        }
        return data;
    }

    getContext2d() {
        let canvas = this.getProxyObject();
        return canvas.getContext('2d');
    }

    getProxyObject() {
        let index = this.getUint32();
        return getProxyObject(index);
    }

    getEnum(name) {
        let index = this.getUint8();
        return enumTable[name][index];
    }
}

function notifyValues(futureId, vals) {
    if (futureId) {
        webSocket.send(JSON.stringify(["notifyResult", futureId, vals, false]));
    }
}

function notifyBinaryData(futureId, data) {
    let sendData = new Uint8Array(data.length + 4);
    let idData = new Uint32Array(1);
    idData[0] = futureId;
    sendData.set(idData, 0);
    sendData.set(data, 4);
    webSocket.send(sendData.buffer);
    webSocket.send(JSON.stringify(["notifyBinaryResult", futureId]));
}

function notifyException(futureId, exception) {
    webSocket.send(JSON.stringify(["notifyResult", futureId, false, exception.toString()]));
}

function notifyEvent(eventType, event) {
    webSocket.send(JSON.stringify(["notifyEvent", eventType, event]));
}

function startApplication() {
    webSocket.send(JSON.stringify(["startApplication"]));
}

/**
 * Graviton binary commands
 */

let binaryCommands = [];

function dispatchBinaryMessage(abuf) {
    let ds = new DataStream(abuf);
    while (ds.hasData()) {
        let commandIndex = ds.getUint16();
        let func = binaryCommands[commandIndex];
        let futureId = false;
        if (commandIndex & 1) {
            futureId = ds.getUint32();
        }
        if (func) {
            try {
                func.apply(null, [futureId, ds]);
            } catch (e) {
                notifyException(futureId, e.toString());
            }
        } else {
            notifyException(false, 'Invalid binary command index: ' + commandIndex);
        }
    }
}

let listenStateTable = {};

/**
 * Graviton text commands
 */

let commandTable = {};

function initializeTextCommandTable() {
    commandTable['registerArgs'] = registerArgs;
}

registerInitializer(initializeTextCommandTable);

function dispatchJsonMessage(params) {
    let command = params[0];
    let args = params.slice(1);
    let func = commandTable[command];
    if (func) {
        func.apply(null, args);
    } else {
        console.log('Invalid parameters: ' + params);
    }
}

function registerArgs(pairs) {
    pairs.forEach((pair) => {
        registerJson(pair[0], pair[1]);
    });
}

function centralizeCanvas(canvas) {
    let winWidth = window.innerWidth;
    let winHeight = window.innerHeight;

    let scaleByWidth = winWidth / canvas.width;
    let scaleByHeight = winHeight / canvas.height;
    let scale = Math.min(scaleByWidth, scaleByHeight);
    let scaledWidth = canvas.width * scale;
    let scaledHeight = canvas.height * scale;
    let canvasTop = (winHeight - scaledHeight) / 2;
    let canvasLeft = (winWidth - scaledWidth) / 2;

    canvas.style.position = 'absolute';
    canvas.style.top = canvasTop + 'px';
    canvas.style.left = canvasLeft + 'px';
    canvas.style.width = scaledWidth + 'px';
    canvas.style.height = scaledHeight + 'px';
}

function obj2style(ctx, style) {
    if (typeof style === 'string') {
        return style;
    } else if (typeof style === 'object') {
        switch (style['type']) {
        case 'linear-gradient':
            let linearGradient = ctx.createLinearGradient(style['x0'], style['y0'], style['x1'], style['y1']);
            style['color-stops'].forEach((colorStop) => {
                linearGradient.addColorStop(colorStop[0], colorStop[1]);
            });
            return linearGradient;
        case 'radial-gradient':
            let radialGradient = ctx.createRadialGradient(style['x0'], style['y0'], style['r0'], style['x1'], style['y1'], style['r1']);
            style['color-stops'].forEach((colorStop) => {
                radialGradient.addColorStop(colorStop[0], colorStop[1]);
            });
            return radialGradient;
        case 'pattern':
            var image = null;
            if (style['canvas']) {
                image = getProxyObject(style['canvas']);
            } else if (style['image']) {
                image = createImageBitmap(getProxyObject(style['image']));
            } else {
                throw new Error('Neither canvas nor image specified for pattern');
            }
            let pattern = ctx.createPattern(image, style['repetition']);
            return pattern;
        default:
            throw new Error('Invalid style: ' + style);
        }
    }
    throw new Error('Invalid style: ' + style);
}


/**
 * Window handler
 */

window.addEventListener('load', () => {
    initializeFunctions.forEach((func) => {
        func();
    });
});

function initializeWindowEvents() {
    ['keydown', 'keyup', 'keypress'].forEach((eventName) => {
        window.addEventListener(eventName, makeEventHandler(eventName));
    });

    if (audioContext.state !== 'suspended') {
        return;
    }

    let eventNames = ['touchstart', 'touchend', 'mousedown', 'keydown'];
    function resumeAudioContext() {
        audioContext.resume().then(removeEvents);
    }
    function removeEvents() {
        eventNames.forEach((eventName) => {
            window.removeEventListener(eventName, resumeAudioContext);
        });
    }
    eventNames.forEach((eventName) => {
        window.addEventListener(eventName, resumeAudioContext);
    });

    window.addEventListener('resize', (event) => {
        let elements = document.getElementById("_on").children;
        for (let i = 0; i < elements.length; ++i) {
            if (elements[i].tagName === "CANVAS") {
                centralizeCanvas(elements[i]);
            }
        }
    });
}

registerInitializer(initializeWindowEvents);

function createMouseEvent(canvas, e) {
    switch (e.toString()) {
    case '[object MouseEvent]':
        return {
            'type': 'MouseEvent',
            'altKey': e.altKey,
            'button': e.button,
            'buttons': e.buttons,
            'clientX': e.clientX,
            'clientY': e.clientY,
            'ctrlKey': e.ctrlKey,
            'metaKey': e.metaKey,
            'movementX': e.movementX,
            'movementY': e.movementY,
            'offsetX': e.offsetX,
            'offsetY': e.offsetY,
            'screenX': e.screenX,
            'screenY': e.screenY,
            'shiftKey': e.shiftKey,
            'canvasX': Math.floor(canvas.width * e.offsetX / canvas.clientWidth),
            'canvasY': Math.floor(canvas.height * e.offsetY / canvas.clientHeight)
        };
    case '[object WheelEvent]':
        return {
            'type': 'WheelEvent',
            'altKey': e.altKey,
            'button': e.button,
            'buttons': e.buttons,
            'clientX': e.clientX,
            'clientY': e.clientY,
            'ctrlKey': e.ctrlKey,
            'metaKey': e.metaKey,
            'movementX': e.movementX,
            'movementY': e.movementY,
            'offsetX': e.offsetX,
            'offsetY': e.offsetY,
            'screenX': e.screenX,
            'screenY': e.screenY,
            'shiftKey': e.shiftKey,
            'deltaX': e.deltaX,
            'deltaY': e.deltaY,
            'deltaZ': e.deltaZ,
            'deltaMode': e.deltaMode,
            'canvasX': Math.floor(canvas.width * e.offsetX / canvas.clientWidth),
            'canvasY': Math.floor(canvas.height * e.offsetY / canvas.clientHeight)
        };
    default:
        throw new Error('Unsupported mouse event: ' + e);
    }
}

function createEvent(e) {
    switch (e.toString()) {
    case '[object KeyboardEvent]':
        return {
            'type': 'KeyboardEvent',
            'altKey': e.altKey,
            'code': e.code,
            'ctrlKey': e.ctrlKey,
            'isComposing': e.isComposing,
            'key': e.key,
            'locale': e.locale,
            'location': e.location,
            'metaKey': e.metaKey,
            'repeat': e.repeat,
            'shiftKey': e.shiftKey
        };
    default:
        throw new Error('Unsupported event: ' + e);
    }
}

function makeEventHandler(eventName) {
    return (e) => {
        if (listenStateTable[eventName]) {
            notifyEvent(eventName, createEvent(e));
        }
    };
}
