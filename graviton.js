'use strict';

let audioContext = new AudioContext();
var webSocket;

function connectServer() {
    webSocket = new WebSocket('ws://' + location.host + '/s');
    webSocket.binaryType = 'arraybuffer';
    webSocket.onopen = () => {
        console.log('opened');
    };
    webSocket.onclose = () => {
        console.log('closed');
        webSocket = null;
        window.close();
    };
    webSocket.onmessage = (event) => {
        if (typeof event.data === 'string') {
            let params =JSON.parse(event.data);
            console.log('received: ' + params);
            dispatchJsonMessage(params);
        } else {
            dispatchBinaryMessage(event.data);
        }
    };
}

let jsonTable = {};
let proxyObjectTable = {};

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

    getUint8Array(len) {
        let data = new Uint8Array(this.dataView.buffer, this.position, len);
        this.position += len;
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
}

function notifyValues(futureId, vals) {
    webSocket.send(JSON.stringify(["notifyResult", futureId, vals, false]));
}

function notifyBinaryData(futureId, data) {
    let sendData = new Uint8Array(data.length + 4);
    let idData = new Uint32Array(1);
    idData[0] = futureId;
    sendData.set(idData, 0);
    sendData.set(data, 4);
    console.log('sendData: ' + sendData);
    webSocket.send(sendData.buffer);
    webSocket.send(JSON.stringify(["notifyBinaryResult", futureId]));
}

function notifyException(futureId, exception) {
    webSocket.send(JSON.stringify(["notifyResult", futureId, false, exception.toString()]));
}

function notifyEvent(eventType, event) {
    webSocket.send(JSON.stringify(["notifyEvent", eventType, event]));
}


/**
 * Graviton binary commands
 */

var binaryCommands = [];

function dispatchBinaryMessage(abuf) {
    let ds = new DataStream(abuf);
    while (ds.hasData()) {
        let commandIndex = ds.getUint16();
        let func = binaryCommands[commandIndex];
        if (func) {
            func.apply(null, [ds]);
        } else {
            console.log('Invalid binary command index: ' + commandIndex);
        }
    }
}

function initializeBinaryCommands() {
    binaryCommands = [
        appClose,

        unlinkProxyObjectCommand,
        registerBinaryData,
        makeCanvasCommand,
        loadCanvasCommand,
        setCanvasVisibilityCommand,
        windowSizeCommand,

        setFillStyleCommand,
        setFontCommand,
        setGlobalAlphaCommand,
        setGlobalCompositeOperationCommand,
        setImageSmoothingEnabledCommand,
        setLineCapCommand,
        setLineDashCommand,
        setLineDashOffsetCommand,
        setLineJoinCommand,
        setLineWidthCommand,
        setMiterLimitCommand,
        setShadowBlurCommand,
        setShadowColorCommand,
        setShadowOffsetXCommand,
        setShadowOffsetYCommand,
        setStrokeStyleCommand,
        setTextAlignCommand,
        setTextBaselineCommand,
        arcCommand,
        arcToCommand,
        beginPathCommand,
        bezierCurveToCommand,
        clearRectCommand,
        clipCommand,
        closePathCommand,
        drawCanvasCommand,
        ellipseCommand,
        fillCommand,
        fillRectCommand,
        fillTextCommand,
        getImageDataCommand,
        isPointInPathCommand,
        isPointInStrokeCommand,
        lineToCommand,
        measureTextCommand,
        moveToCommand,
        putImageDataCommand,
        quadraticCurveToCommand,
        rectCommand,
        restoreCommand,
        rotateCommand,
        saveCommand,
        scaleCommand,
        setTransformCommand,
        strokeCommand,
        strokeRectCommand,
        strokeTextCommand,
        transformCommand,
        translateCommand,
        createImageDataCommand,
        uploadImageDataCommand,
        downloadImageDataCommand,

        listenWindowEvent,
        listenCanvasEvent,

        setAudioBaseTime,
        startAudioNode,
        stopAudioNode,
        connectNode,
        disconnectNode,
        audioNodeEnd,
        audioParamSetValueAtTime,
        audioParamLinearRampToValueAtTime,
        audioParamExponentialRampToValueAtTime,
        audioParamSetTargetAtTime,
        audioParamSetValueCurveAtTime,
        audioParamCancelScheduledValues,
        audioParamCancelAndHoldAtTime,
        popAudioParam,
        makeAudioContextDestination,
        makeOscillatorNode,
        setOscillatorType,
        pushOscillatorFrequencyAudioParam,
        pushOscillatorDetuneAudioParam,
        setOscillatorPeriodicWave,
        loadAudio,
        playAudio,
        pauseAudio,
        loadPCM,
        makeAudioBufferSourceNode,
        pushAudioBufferSourceNodeDetuneAudioParam,
        pushAudioBufferSourceNodePlaybackRateAudioParam,
        setAudioBufferSourceNodeLoop
    ];
}

function appClose(ds) {
    if (webSocket) {
        webSocket.close(1000);
    }
}

function unlinkProxyObjectCommand(ds) {
    let index = ds.getUint32();
    unlinkProxyObject(index);
}

let binaryDataTable = {};

function registerBinaryData(ds) {
    let index = ds.getUint32();
    let len = ds.getUint32();
    let binaryData = ds.getUint8Array(len);
    binaryDataTable[index] = binaryData;
}

function unregisterBinaryData(index) {
    binaryDataTable[index] = null;
}

let listenStateTable = {};

function listenWindowEvent(ds) {
    let eventType = ds.getJson();
    let flag = ds.getBoolean();
    listenStateTable[eventType] = flag;
}

function listenCanvasEvent(ds) {
    let canvas = ds.getProxyObject();
    let eventType = ds.getJson();
    let eventName = ds.getJson();
    let flag = ds.getBoolean();

    let handler = flag ? (e) => { notifyEvent(eventName, createMouseEvent(canvas, e)); } : null;

    switch (eventType) {
    case 'click':
        canvas.onclick = handler;
        break;
    case 'dblclick':
        canvas.ondblclick = handler;
        break;
    case 'contextmenu':
        canvas.oncontextmenu = handler;
        break;
    case 'mousedown':
        canvas.onmousedown = handler;
        break;
    case 'mouseup':
        canvas.onmouseup = handler;
        break;
    case 'mouseover':
        canvas.mouseover = handler;
        break;
    case 'mouseout':
        canvas.mouseout = handler;
        break;
    case 'mousemove':
        canvas.mousemove = handler;
        break;
    case 'wheel':
        canvas.onwheel = handler;
        break;
    default:
        throw new Error('Unsupported event: ' + eventType);
    }
}

function makeCanvas(canvasId, width, height, z, visibility) {
    let canvas = document.createElement('canvas');
    canvas.id = 'canvas' + canvasId;
    canvas.width = width;
    canvas.height = height;
    linkProxyObject(canvasId, canvas);

    centralizeCanvas(canvas);
    canvas.style.zIndex = z;
    document.getElementById('_on').appendChild(canvas);

    if (window.isElectron) {
        window.showBrowserWindow();
    }

    if (visibility) {
        canvas.style.visibility = 'visible';
    } else {
        canvas.style.visibility = 'hidden';
    }

    return canvas;
}

function makeCanvasCommand(ds) {
    let canvasId = ds.getUint32();
    let width = ds.getUint32();
    let height = ds.getUint32();
    let z = ds.getUint32();
    let visibility = ds.getBoolean();
    makeCanvas(canvasId, width, height, z, visibility);
}

function loadCanvasCommand(ds) {
    let futureId = ds.getUint32();
    let canvasId = ds.getUint32();
    let url = ds.getJson();
    let z = ds.getUint32();
    let visibility = ds.getBoolean();

    let img = new Image();
    workingImages.add(img);
    img.src = url;
    img.onload = () => {
        let canvas = makeCanvas(canvasId, img.width, img.height, z, visibility);
        let ctx = canvas.getContext('2d');
        ctx.drawImage(img, 0, 0);
        workingImages.delete(img);
        notifyValues(futureId, [canvas.width, canvas.height]);
    };
    img.onerror = () => {
        console.log('Load image failed: ' + url);
        workingImages.delete(img);
        notifyException(futureId, "Load image failed.");
    };
}

function setCanvasVisibilityCommand(ds) {
    let canvas = ds.getProxyObject();
    let visibility = ds.getBoolean();

    if (visibility) {
        canvas.style.visibility = 'visible';
        if (window.isElectron) {
            window.showBrowserWindow();
        }
    } else {
        canvas.style.visibility = 'hidden';
    }
}

function windowSizeCommand(ds) {
    let futureId = ds.getUint32();
    let winWidth = window.innerWidth;
    let winHeight = window.innerHeight;
    notifyValues(futureId, [winWidth, winHeight]);
}

/**
 * Draw commands
 */

let workingImages = new Set();

function setFillStyleCommand(ds) {
    let ctx = ds.getContext2d();
    let style = ds.getJson();
    ctx.fillStyle = obj2style(ctx, style);
}

function setFontCommand(ds) {
    let ctx = ds.getContext2d();
    let font = ds.getJson();
    ctx.font = font;
}

function setGlobalAlphaCommand(ds) {
    let ctx = ds.getContext2d();
    let alpha = ds.getFloat64();
    ctx.globalAlpha = alpha;
}

function setGlobalCompositeOperationCommand(ds) {
    let ctx = ds.getContext2d();
    let op = ds.getJson();
    ctx.globalCompositeOperation = op;
}

function setImageSmoothingEnabledCommand(ds) {
    let ctx = ds.getContext2d();
    let val = ds.getBoolean();
    ctx.setImageSmoothingEnabled = val;
}

function setLineCapCommand(ds) {
    let ctx = ds.getContext2d();
    let val = ds.getJson();
    ctx.lineCap = val;
}

function setLineDashCommand(ds) {
    let ctx = ds.getContext2d();
    let segments = ds.getJson();
    ctx.setLineDash(segments);
}

function setLineDashOffsetCommand(ds) {
    let ctx = ds.getContext2d();
    let offset = ds.getFloat64();
    ctx.lineDashOffset = offset;
}

function setLineJoinCommand(ds) {
    let ctx = ds.getContext2d();
    let val = ds.getJson();
    ctx.lineJoin = val;
}

function setLineWidthCommand(ds) {
    let ctx = ds.getContext2d();
    let w = ds.getFloat64();
    ctx.lineWidth = w;
}

function setMiterLimitCommand(ds) {
    let ctx = ds.getContext2d();
    let limit = ds.getFloat64();
    ctx.miterLimit = limit;
}

function setShadowBlurCommand(ds) {
    let ctx = ds.getContext2d();
    let level = ds.getFloat64();
    ctx.shadowBlur = level;
}

function setShadowColorCommand(ds) {
    let ctx = ds.getContext2d();
    let color = ds.getJson();
    ctx.shadowColor = color;
}

function setShadowOffsetXCommand(ds) {
    let ctx = ds.getContext2d();
    let offset = ds.getFloat64();
    ctx.shadowOffsetX = offset;
}

function setShadowOffsetYCommand(ds) {
    let ctx = ds.getContext2d();
    let offset = ds.getFloat64();
    ctx.shadowOffsetY = offset;
}

function setStrokeStyleCommand(ds) {
    let ctx = ds.getContext2d();
    let style = ds.getJson();
    ctx.strokeStyle = obj2style(ctx, style);
}

function setTextAlignCommand(ds) {
    let ctx = ds.getContext2d();
    let align = ds.getJson();
    ctx.textAlign = align;
}

function setTextBaselineCommand(ds) {
    let ctx = ds.getContext2d();
    let val = ds.getJson();
    ctx.textBaseline = val;
}

function arcCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let radius = ds.getInt32();
    let startAngle = ds.getFloat64();
    let endAngle = ds.getFloat64();
    let antiClockwise = ds.getBoolean();
    ctx.arc(x, y, radius, startAngle, endAngle, antiClockwise);
}

function arcToCommand(ds) {
    let ctx = ds.getContext2d();
    let x1 = ds.getInt32();
    let y1 = ds.getInt32();
    let x2 = ds.getInt32();
    let y2 = ds.getInt32();
    let radius = ds.getInt32();
    ctx.arcTo(x1, y1, x2, y2, radius);
}

function beginPathCommand(ds) {
    let ctx = ds.getContext2d();
    ctx.beginPath();
}

function bezierCurveToCommand(ds) {
    let ctx = ds.getContext2d();
    let cp1x = ds.getInt32();
    let cp1y = ds.getInt32();
    let cp2x = ds.getInt32();
    let cp2y = ds.getInt32();
    let x = ds.getInt32();
    let y = ds.getInt32();
    ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y);
}

function clearRectCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let w = ds.getInt32();
    let h = ds.getInt32();
    ctx.clearRect(x, y, w, h);
}

function clipCommand(ds) {
    let ctx = ds.getContext2d();
    let rule = ds.getJson();
    ctx.clip(rule);
}

function closePathCommand(ds) {
    let ctx = ds.getContext2d();
    ctx.closePath();
}

function drawCanvasCommand(ds) {
    let ctx = ds.getContext2d();
    let srcCanvas = ds.getProxyObject();
    let mode = ds.getUint8();
    if (mode === 1) {
        let sx = ds.getInt32();
        let sy = ds.getInt32();
        let sw = ds.getInt32();
        let sh = ds.getInt32();
        let dx = ds.getInt32();
        let dy = ds.getInt32();
        let dw = ds.getInt32();
        let dh = ds.getInt32();
        ctx.drawImage(srcCanvas, sx, sy, sw, sh, dx, dy, dw, dh);
    } else if (mode === 2) {
        let dx = ds.getInt32();
        let dy = ds.getInt32();
        let dw = ds.getInt32();
        let dh = ds.getInt32();
        ctx.drawImage(srcCanvas, dx, dy, dw, dh);
    } else {
        let dx = ds.getInt32();
        let dy = ds.getInt32();
        ctx.drawImage(srcCanvas, dx, dy);
    }
}

function ellipseCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let radiusX = ds.getInt32();
    let radiusY = ds.getInt32();
    let rotation = ds.getFloat64();
    let startAngle = ds.getFloat64();
    let endAngle = ds.getFloat64();
    let antiClockwise = ds.getBoolean();
    ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, antiClockwise);
}

function fillCommand(ds) {
    let ctx = ds.getContext2d();
    let rule = ds.getJson();
    ctx.fill(rule);
}

function fillRectCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let w = ds.getInt32();
    let h = ds.getInt32();
    ctx.fillRect(x, y, w, h);
}

function fillTextCommand(ds) {
    let ctx = ds.getContext2d();
    let text = ds.getJson();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let maxWidth = ds.getInt32();
    if (maxWidth > 0) {
        ctx.fillText(text, x, y, maxWidth);
    } else {
        ctx.fillText(text, x, y);
    }
}

function getImageDataCommand(ds) {
    let ctx = ds.getContext2d();
    let imageId = ds.getUint32();
    let sx = ds.getInt32();
    let sy = ds.getInt32();
    let sw = ds.getInt32();
    let sh = ds.getInt32();
    let image = ctx.getImageData(sx, sy, sw, sh);
    linkProxyObject(imageId, image);
}

function isPointInPathCommand(ds) {
    let ctx = ds.getContext2d();
    let futureId = ds.getUint32();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let fillRule = ds.getJson();
    let result = ctx.isPointInPath(x, y, fillRule);
    notifyValues(futureId, [result]);
}

function isPointInStrokeCommand(ds) {
    let ctx = ds.getContext2d();
    let futureId = ds.getUint32();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let result = ctx.isPointInStroke(x, y);
    notifyValues(futureId, [result]);
}

function lineToCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    ctx.lineTo(x, y);
}

function measureTextCommand(ds) {
    let ctx = ds.getContext2d();
    let futureId = ds.getUint32();
    let text = ds.getJson();
    let textMetrics = ctx.measureText(text);
    notifyValues(futureId, [{"width": textMetrics.width}]);
}

function moveToCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    ctx.moveTo(x, y);
}

function putImageDataCommand(ds) {
    let ctx = ds.getContext2d();
    let image = ds.getProxyObject();
    let dx = ds.getInt32();
    let dy = ds.getInt32();
    let useDirty = ds.getBoolean();
    if (useDirty) {
        let dirtyX = ds.getInt32();
        let dirtyY = ds.getInt32();
        let dirtyWidth = ds.getInt32();
        let dirtyHeight = ds.getInt32();
        ctx.putImageData(image, dx, dy, dirtyX, dirtyY, dirtyWidth, dirtyHeight);
    } else {
        ctx.putImageData(image, dx, dy);
    }
}

function quadraticCurveToCommand(ds) {
    let ctx = ds.getContext2d();
    let cpx = ds.getInt32();
    let cpy = ds.getInt32();
    let x = ds.getInt32();
    let y = ds.getInt32();
    ctx.quadraticCurveTo(cpx, cpy, x, y);
}

function rectCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let w = ds.getInt32();
    let h = ds.getInt32();
    ctx.rect(x, y, w, h);
}

function restoreCommand(ds) {
    let ctx = ds.getContext2d();
    ctx.restore();
}

function rotateCommand(ds) {
    let ctx = ds.getContext2d();
    let angle = ds.getFloat64();
    ctx.rotate(angle);
}

function saveCommand(ds) {
    let ctx = ds.getContext2d();
    ctx.save();
}

function scaleCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getFloat64();
    let y = ds.getFloat64();
    ctx.scale(x, y);
}

function setTransformCommand(ds) {
    let ctx = ds.getContext2d();
    let a = ds.getFloat64();
    let b = ds.getFloat64();
    let c = ds.getFloat64();
    let d = ds.getFloat64();
    let e = ds.getFloat64();
    let f = ds.getFloat64();
    ctx.setTransform(a, b, c, d, e, f);
}

function strokeCommand(ds) {
    let ctx = ds.getContext2d();
    ctx.stroke();
}

function strokeRectCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let w = ds.getInt32();
    let h = ds.getInt32();
    ctx.strokeRect(x, y, w, h);
}

function strokeTextCommand(ds) {
    let ctx = ds.getContext2d();
    let text = ds.getJson();
    let x = ds.getInt32();
    let y = ds.getInt32();
    let maxWidth = ds.getInt32();
    if (maxWidth > 0) {
        ctx.strokeText(text, x, y, maxWidth);
    } else {
        ctx.strokeText(text, x, y);
    }
}

function transformCommand(ds) {
    let ctx = ds.getContext2d();
    let a = ds.getFloat64();
    let b = ds.getFloat64();
    let c = ds.getFloat64();
    let d = ds.getFloat64();
    let e = ds.getFloat64();
    let f = ds.getFloat64();
    ctx.transform(a, b, c, d, e, f);
}

function translateCommand(ds) {
    let ctx = ds.getContext2d();
    let x = ds.getInt32();
    let y = ds.getInt32();
    ctx.translate(x, y);
}

function createImageDataCommand(ds) {
    let ctx = ds.getContext2d();
    let imageId = ds.getUint32();
    let w = ds.getInt32();
    let h = ds.getInt32();
    let image = ctx.createImageData(w, h);
    linkProxyObject(imageId, image);
}

function uploadImageDataCommand(ds) {
    let image = ds.getProxyObject();
    let len = ds.getUint32();
    let imageData = ds.getUint8Array(len);
    image.data.set(imageData);
}

function downloadImageDataCommand(ds) {
    let futureId = ds.getUint32();
    let image = ds.getProxyObject();
    notifyBinaryData(futureId, image.data);
}


/**
 * Audio commands
 */

let audioBaseTime = audioContext.currentTime;

function setAudioBaseTime(ds) {
    audioBaseTime = audioContext.currentTime;
}

function startAudioNode(ds) {
    let audioNode = ds.getProxyObject();
    let deltaWhen = ds.getFloat64();
    let offset = ds.getFloat64();
    let duration = ds.getFloat64();

    let when = 0;
    if (deltaWhen > 0) {
        when = audioBaseTime + deltaWhen;
    }
    if (duration < 0) {
        audioNode.start(when, offset);
    } else {
        audioNode.start(when, offset, duration);
    }
}

function stopAudioNode(ds) {
    let audioNode = ds.getProxyObject();
    let deltaWhen = ds.getFloat64();

    let when = 0;
    if (deltaWhen > 0) {
        when = audioBaseTime + deltaWhen;
    }
    audioNode.stop(when);
}

function connectNode(ds) {
    let fromAudioNode = ds.getProxyObject();
    let toAudioNode = ds.getProxyObject();
    fromAudioNode.connect(toAudioNode);
}

function disconnectNode(ds) {
    let fromAudioNode = ds.getProxyObject();
    let toAudioNode = ds.getProxyObject();
    fromAudioNode.disconnect(toAudioNode);
}

function audioNodeEnd(ds) {
    let futureId = ds.getUint32();
    let audioNode = ds.getProxyObject();

    audioNode.onended = (event) => {
        notifyValues(futureId, [true]);
    };
}

function makeAudioContextDestination(ds) {
    let nodeId = ds.getUint32();
    linkProxyObject(nodeId, audioContext.destination);
}

let audioParamStack = [];

function audioParamSetValueAtTime(ds) {
    let value = ds.getFloat64();
    let startTime = audioBaseTime + ds.getFloat64();
    audioParamStack[0].setValueAtTime(value, startTime);
}

function audioParamLinearRampToValueAtTime(ds) {
    let value = ds.getFloat64();
    let endTime = audioBaseTime + ds.getFloat64();
    audioParamStack[0].linearRampToValueAtTime(value, endTime);
}

function audioParamExponentialRampToValueAtTime(ds) {
    let value = ds.getFloat64();
    let endTime = audioBaseTime + ds.getFloat64();
    audioParamStack[0].exponentialRampToValueAtTime(value, endTime);
}

function audioParamSetTargetAtTime(ds) {
    let value = ds.getFloat64();
    let startTime = audioBaseTime + ds.getFloat64();
    let timeConstant = ds.getFloat64();
    audioParamStack[0].setTargetAtTime(value, startTime, timeConstant);
}

function audioParamSetValueCurveAtTime(ds) {
    let values = ds.getJson();
    let startTime = audioBaseTime + ds.getFloat64();
    let duration = ds.getFloat64();
    audioParamStack[0].setValueCurveAtTime(values, startTime, duration);
}

function audioParamCancelScheduledValues(ds) {
    let startTime = audioBaseTime + ds.getFloat64();
    audioParamStack[0].cancelScheduledValues(startTime);
}

function audioParamCancelAndHoldAtTime(ds) {
    let cancelTime = audioBaseTime + ds.getFloat64();
    audioParamStack[0].cancelAndHoldAtTime(cancelTime);
}

function popAudioParam(ds) {
    audioParamStack.shift();
}

function makeOscillatorNode(ds) {
    let nodeId = ds.getUint32();

    let oscillatorNode = new OscillatorNode(audioContext);
    linkProxyObject(nodeId, oscillatorNode);
}

function setOscillatorType(ds) {
    let audioNode = ds.getProxyObject();
    let type = ds.getJson();
    audioNode.type = type;
}

function pushOscillatorFrequencyAudioParam(ds) {
    let audioNode = ds.getProxyObject();
    audioParamStack.unshift(audioNode.frequency);
}

function pushOscillatorDetuneAudioParam(ds) {
    let audioNode = ds.getProxyObject();
    audioParamStack.unshift(audioNode.detune);
}

function setOscillatorPeriodicWave(ds) {
    let oscillatorNode = ds.getProxyObject();
    let real = ds.getJson();
    let imag = ds.getJson();
    let constraints = ds.getJson();
    oscillatorNode.setPeriodicWave(audioContext.createPeriodicWave(real, imag, constraints));
}

function loadAudio(ds) {
    let futureId = ds.getUint32();
    let nodeId = ds.getUint32();
    let url = ds.getJson();

    let audio = new Audio(url);
    audio.onloadedmetadata = () => {
        let sourceNode = audioContext.createMediaElementSource(audio);
        linkProxyObject(nodeId, sourceNode);
        sourceNode.connect(audioContext.destination);
        notifyValues(futureId, [audio.duration]);
    };
    audio.onstalled = () => {
        notifyException(futureId, "Load audio failed.");
    };
}

function playAudio(ds) {
    let sourceNode = ds.getProxyObject();
    sourceNode.mediaElement.play();
}

function pauseAudio(ds) {
    let sourceNode = ds.getProxyObject();
    sourceNode.mediaElement.pause();
}

function loadPCM(ds) {
    let futureId = ds.getUint32();
    let objectId = ds.getUint32();
    let url = ds.getJson();

    let req = new XMLHttpRequest();
    req.open('GET', url, true);
    req.responseType = 'arraybuffer';
    req.onload = () => {
        if (req.status === 200) {
            let buf = req.response;
            audioContext.decodeAudioData(buf).then(
                (decodedData) => {
                    linkProxyObject(objectId, decodedData);
                    notifyValues(futureId, [decodedData.sampleRate,
                                            decodedData.length,
                                            decodedData.duration,
                                            decodedData.numberOfChannels]);
                },
                (reason) => {
                    notifyException(futureId, reason);
                });
        }
    };
    req.send();
}

function makeAudioBufferSourceNode(ds) {
    let nodeId = ds.getUint32();
    let pcmData = ds.getProxyObject();

    let sourceNode = audioContext.createBufferSource();
    sourceNode.buffer = pcmData;
    linkProxyObject(nodeId, sourceNode);
}

function pushAudioBufferSourceNodeDetuneAudioParam(ds) {
    let sourceNode = ds.getProxyObject();
    audioParamStack.push(sourceNode.detune);
}

function pushAudioBufferSourceNodePlaybackRateAudioParam(ds) {
    let sourceNode = ds.getProxyObject();
    audioParamStack.push(sourceNode.playbackRate);
}

function setAudioBufferSourceNodeLoop(ds) {
    let sourceNode = ds.getProxyObject();
    let loop = ds.getBoolean();
    let loopStart = ds.getFloat64();
    let loopEnd = ds.getFloat64();

    sourceNode.loop = loop;
    if (loop) {
        sourceNode.loopStart = loopStart;
        sourceNode.loopEnd = loopEnd;
    }
}

/**
 * Graviton text commands
 */

let commandTable = {};

function initializeTextCommandTable() {
    commandTable['registerArgs'] = registerArgs;
}

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
    console.log(style);
    if (typeof style === 'string') {
        return style;
    } else if (typeof style === 'object') {
        console.log('object style');
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

window.onload = () => {
    initializeBinaryCommands();
    initializeTextCommandTable();
    initializeWindowEvents();
    connectServer();
};

window.onresize = (e) => {
    let elements = document.getElementById('_on').children;
    for (var i = 0; i < elements.length; i++) {
        if (elements[i].tagName === 'CANVAS') {
            centralizeCanvas(elements[i]);
        }
    }
};

function initializeWindowEvents() {
    ['keydown', 'keyup', 'keypress'].forEach((eventName) => {
        window.addEventListener(eventName, makeEventHandler(eventName));
    });

    console.log('initial AudioContext: ' + audioContext.state);
    if (audioContext.state !== 'suspended') {
        return;
    }

    let eventNames = ['touchstart', 'touchend', 'mousedown', 'keydown'];
    function resumeAudioContext() {
        audioContext.resume().then(removeEvents);
    }
    function removeEvents() {
        console.log('AudioContext: ' + audioContext.state);
        eventNames.forEach((eventName) => {
            window.removeEventListener(eventName, resumeAudioContext);
        });
    }
    eventNames.forEach((eventName) => {
        window.addEventListener(eventName, resumeAudioContext);
    });
}

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
