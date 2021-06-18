'use strict';

let webSocket;

function connectServer() {
    webSocket = new WebSocket('ws://' + location.host + '/_s' + location.pathname + location.search);
    webSocket.binaryType = 'arraybuffer';
    webSocket.onopen = () => {
        console.log('opened');
    };
    webSocket.onclose = () => {
        console.log('closed');
        webSocket = null;
        if (window.grvCloseThisWindow) {
            window.grvCloseThisWindow();
        } else {
            window.close();
        }
    };
    webSocket.onmessage = (event) => {
        try {
            if (typeof event.data === 'string') {
                notifyException(new Error("Can't handle string data: " + event.data));
            } else {
                dispatchBinaryMessage(event.data);
            }
        } catch (e) {
            notifyException(e);
        }
    };
}

window.addEventListener('load', connectServer);

export function closeConnection() {
    if (webSocket) {
        webSocket.close(1000);
    }
}

const classIdTable = new Map();
const objectTable = new Map();
let objectNextId = 0;

function lookupObject(objectId) {
    const obj = objectTable.get(objectId);
    if (obj === undefined) {
        throw `Object not found: ${objectId}`;
    }

    return obj;
}

export function registerClass(classId, className) {
    classIdTable.set(className, classId);
}

function findClassId(obj) {
    let klass = obj.constructor;
    while (klass.name) {
        let classId = classIdTable.get(klass.name);
        if (classId !== undefined) {
            return classId;
        }
        klass = Object.getPrototypeOf(klass);
    }
    return classIdTable.get('Object');
}

function findObjectId(obj) {
    let objectId;
    do {
        objectId = objectNextId++;
    } while (objectTable.get(objectId));
    objectTable.set(objectId, obj);
    return objectId;
}

export function freeObjectId(objectId) {
    objectTable.delete(objectId);
    if (objectId < objectNextId) {
        objectNextId = objectId;
    }
}

const enumTable = new Map();

export function registerEnum(enumName, vals) {
    enumTable.set(enumName, vals);
}

function extractValueWithProperties(obj, props) {
    if (props.length === 0) {
        return obj;
    }

    const val = obj[props[0]];
    const rest = props.slice(1);
    if (Number.isInteger(val.length)) {
        return Array.from(val).map((v) => extractPropertyValue(v, rest));
    } else {
        return extractPropertyValue(val, rest);
    }
}

function convertObjectToArray(obj, arrayRule) {
    const result = [];
    arrayRule.forEach((propSpec) => {
        if (propSpec instanceof Array) {
            result.push(extractValueWithProperties(obj, propSpec));
        } else {
            result.push(obj[propSpec]);
        }
    });
    return result;
}

function convertObjectToObject(obj, objectRule) {
    const result = {};
    Object.keys(objectRule).forEach((k) => {
        const rule = objectRule[k];
        if (rule === true) {
            result[k] = obj[k];
        } else if (typeof rule === 'string') {
            result[k] = obj[rule];
        } else if (rule instanceof Array) {
            const [elementKey, elementRule] = rule;
            result[k] = convertObjectToObject(obj[elementKey], elementRule);
        } else {
            result[k] = convertObjectToObject(obj[k], rule);
        }
    });
    return result;
}

function convertObject(obj, rule) {
    if (rule instanceof Array) {
        return convertObjectToArray(obj, rule);
    } else {
        return convertObjectToObject(obj, rule);
    }
}

class JSONValue {
    constructor(obj) {
        this.value = obj;
    }
}

export function jsonValue(obj, rule) {
    return new JSONValue(rule ? convertObject(obj, rule) : obj);
}

const procedureTable = new Map();

function registerProcedure(futureId, proc) {
    procedureTable.set(futureId, proc);
}

export function freeProcedure(futureId) {
    procedureTable.delete(futureId);
}

function findProcedure(futureId) {
    return procedureTable.get(futureId);
}

const VAL_TYPE_UNDEFINED = 1;
const VAL_TYPE_NULL = 2;
const VAL_TYPE_TRUE = 3;
const VAL_TYPE_FALSE = 4;
const VAL_TYPE_PROCEDURE = 5;
const VAL_TYPE_POSITIVE_INFINITY = 6;
const VAL_TYPE_NEGATIVE_INFINITY = 7;
const VAL_TYPE_NAN = 8;
const VAL_TYPE_STRING = 9;
const VAL_TYPE_SYMBOL = 10;
const VAL_TYPE_OBJECT = 11;
const VAL_TYPE_DATE = 12;
const VAL_TYPE_ARRAY = 13;
const VAL_TYPE_INT8ARRAY = 14;
const VAL_TYPE_UINT8ARRAY = 15;
const VAL_TYPE_INT16ARRAY = 16;
const VAL_TYPE_UINT16ARRAY = 17;
const VAL_TYPE_INT32ARRAY = 18;
const VAL_TYPE_UINT32ARRAY = 19;
const VAL_TYPE_FLOAT32ARRAY = 20;
const VAL_TYPE_FLOAT64ARRAY = 21;
const VAL_TYPE_JSON = 22;
const VAL_TYPE_INT8 = 23;
const VAL_TYPE_UINT8 = 24;
const VAL_TYPE_INT16 = 25;
const VAL_TYPE_UINT16 = 26;
const VAL_TYPE_INT32 = 27;
const VAL_TYPE_UINT32 = 28;
const VAL_TYPE_FLOAT64 = 29;

const IS_LITTLE_ENDIAN = (new Uint8Array(Uint16Array.of(1).buffer)[0] === 1);

class DataReadStream {
    constructor(buf) {
        this.dataView = new DataView(buf);
        this.position = 0;
        this.textDecoder = new TextDecoder('utf-8');
    }

    hasData() {
        return this.position < this.dataView.byteLength;
    }

    getFloat32() {
        const v = this.dataView.getFloat32(this.position, true);
        this.position += 4;
        return v;
    }

    getFloat64() {
        const v = this.dataView.getFloat64(this.position, true);
        this.position += 8;
        return v;
    }

    getInt16() {
        const v = this.dataView.getInt16(this.position, true);
        this.position += 2;
        return v;
    }

    getInt32() {
        const v = this.dataView.getInt32(this.position, true);
        this.position += 4;
        return v;
    }

    getInt8() {
        const v = this.dataView.getInt8(this.position);
        this.position += 1;
        return v;
    }

    getUint16() {
        const v = this.dataView.getUint16(this.position, true);
        this.position += 2;
        return v;
    }

    getUint32() {
        const v = this.dataView.getUint32(this.position, true);
        this.position += 4;
        return v;
    }

    getUint8() {
        const v = this.dataView.getUint8(this.position);
        this.position += 1;
        return v;
    }

    getBoolean() {
        const v = this.getUint8();
        return v !== 0;
    }

    getString() {
        const data = this.getUint8Array();
        return this.textDecoder.decode(data);
    }

    getJson() {
        const len = this.getAny();
        const obj = {};
        for (let i = 0; i < len; ++i) {
            const key = this.getAny();
            const val = this.getAny();
            obj[key] = val;
        }
        return obj;
    }

    getInt8Array() {
        const len = this.getAny();
        const data = new Int8Array(this.dataView.buffer, this.position, len);
        this.position += len;
        return data;
    }

    getUint8Array() {
        const len = this.getAny();
        const data = new Uint8Array(this.dataView.buffer, this.position, len);
        this.position += len;
        return data;
    }

    decodeTypedArray(getter, typedArrayClass) {
        const len = this.getAny();
        if (IS_LITTLE_ENDIAN) {
            const byteLen = len * typedArrayClass.BYTES_PER_ELEMENT;
            const buf = this.dataView.buffer.slice(this.position, this.position + byteLen);
            const data = new typedArrayClass(buf, 0, len);
            this.position += byteLen;
            return data;
        } else {
            const data = new typedArrayClass(len);
            for (let i = 0; i < len; ++i) {
                data[i] = getter();
            }
            return data;
        }
    }

    getInt16Array() {
        return this.decodeTypedArray(this.getInt16, Int16Array);
    }

    getUint16Array() {
        return this.decodeTypedArray(this.getUint16, Uint16Array);
    }

    getInt32Array() {
        return this.decodeTypedArray(this.getInt32, Int32Array);
    }

    getUint32Array() {
        return this.decodeTypedArray(this.getUint32, Uint32Array);
    }

    getFloat32Array() {
        return this.decodeTypedArray(this.getFloat32, Float32Array);
    }

    getFloat64Array() {
        return this.decodeTypedArray(this.getFloat64, Float64Array);
    }

    getObject() {
        const objectId = this.getAny();
        return lookupObject(objectId);
    }

    getEnum(name) {
        const index = this.getUint8();
        return enumTable.get(name)[index];
    }

    getSymbol() {
        return Symbol(this.getString());
    }

    getDate() {
        const ms = this.getAny();
        return new Date(ms);
    }

    getArray() {
        const len = this.getAny();
        const array = [];
        for (let i = 0; i < len; ++i) {
            array.push(this.getAny());
        }
        return array;
    }

    getProcedure() {
        const futureId = this.getUint32();
        let proc = findProcedure(futureId);
        if (!proc) {
            proc = (...args) => {
                notifyValues(futureId, ...args);
            };
            registerProcedure(futureId, proc);
        }
        return proc;
    }

    getAny() {
        const valType = this.getInt8();
        if (valType <= 0) {
            return -valType;
        }

        switch (valType) {
            case VAL_TYPE_UNDEFINED:
                return undefined;
            case VAL_TYPE_NULL:
                return null;
            case VAL_TYPE_TRUE:
                return true;
            case VAL_TYPE_FALSE:
                return false;
            case VAL_TYPE_POSITIVE_INFINITY:
                return Infinity;
            case VAL_TYPE_NEGATIVE_INFINITY:
                return -Infinity;
            case VAL_TYPE_NAN:
                return NaN;
            case VAL_TYPE_STRING:
                return this.getString();
            case VAL_TYPE_SYMBOL:
                return this.getSymbol();
            case VAL_TYPE_OBJECT:
                return this.getObject();
            case VAL_TYPE_DATE:
                return this.getDate();
            case VAL_TYPE_ARRAY:
                return this.getArray();
            case VAL_TYPE_INT8ARRAY:
                return this.getInt8Array();
            case VAL_TYPE_UINT8ARRAY:
                return this.getUint8Array();
            case VAL_TYPE_INT16ARRAY:
                return this.getInt16Array();
            case VAL_TYPE_UINT16ARRAY:
                return this.getUint16Array();
            case VAL_TYPE_INT32ARRAY:
                return this.getInt32Array();
            case VAL_TYPE_UINT32ARRAY:
                return this.getUint32Array();
            case VAL_TYPE_FLOAT32ARRAY:
                return this.getFloat32Array();
            case VAL_TYPE_FLOAT64ARRAY:
                return this.getFloat64Array();
            case VAL_TYPE_JSON:
                return this.getJson();
            case VAL_TYPE_INT8:
                return this.getInt8();
            case VAL_TYPE_UINT8:
                return this.getUint8();
            case VAL_TYPE_INT16:
                return this.getInt16();
            case VAL_TYPE_UINT16:
                return this.getUint16();
            case VAL_TYPE_INT32:
                return this.getInt32();
            case VAL_TYPE_UINT32:
                return this.getUint32();
            case VAL_TYPE_FLOAT64:
                return this.getFloat64();
            case VAL_TYPE_PROCEDURE:
                return this.getProcedure();
            default:
                throw `Unsupported value type: ${valType}`;
        }
    }
}

function encodeValueWithString(valType, str) {
    const textEncoder = new TextEncoder();
    const strData = textEncoder.encode(str);
    const strLen = strData.byteLength;
    const strLenData = encodeNumber(strLen);
    const data = new Uint8Array(1 + strLenData.byteLength + strData.byteLength);
    const dataView = new DataView(data.buffer);
    dataView.setUint8(0, valType);
    data.set(strLenData, 1);
    data.set(strData, 1 + strLenData.byteLength);
    return data;
}

function encodeNumber(v) {
    if (Number.isInteger(v)) {
        if (0 <= v) {
            if (v <= 0x80) {
                return Int8Array.of(-v);
            } else if (v <= 0xff) {
                // Uint8
                const data = new Uint8Array(1 + 1);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT8);
                dataView.setUint8(1, v);
                return data;
            } else if (v <= 0xffff) {
                // Uint16
                const data = new Uint8Array(1 + 2);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT16);
                dataView.setUint16(1, v, true);
                return data;
            } else if (v <= 0xffffffff) {
                // Uint32
                const data = new Uint8Array(1 + 4);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT32);
                dataView.setUint32(1, v, true);
                return data;
            }
            // fallback to float64
        } else {
            if (-0x80 <= v) {
                // Int8
                const data = new Uint8Array(1 + 1);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT8);
                dataView.setInt8(1, v);
                return data;
            } else if (-0x8000 <= v) {
                // Int16
                const data = new Uint8Array(1 + 2);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT16);
                dataView.setInt16(1, v, true);
                return data;
            } else if (-0x80000000 <= v) {
                // Int32
                const data = new Uint8Array(1 + 4);
                const dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT32);
                dataView.setInt32(1, v, true);
                return data;
            }
            // fallback to float64
        }
    }
    // Float64
    const data = new Uint8Array(1 + 8);
    const dataView = new DataView(data.buffer);
    dataView.setInt8(0, VAL_TYPE_FLOAT64);
    dataView.setFloat64(1, v, true);
    return data;
}

function encodeTypedArray(valType, typedArray, filler) {
    const lenData = encodeNumber(typedArray.byteLength);
    const data = new Uint8Array(1 + lenData.byteLength + typedArray.byteLength);
    const dataView = new DataView(data.buffer);
    dataView.setInt8(0, valType);
    data.set(lenData, 1);

    let offset = 1 + lenData.byteLength;
    if (IS_LITTLE_ENDIAN) {
        data.set(typedArray, offset);
    } else {
        const setVal = filler.bind(dataView);
        typedArray.forEach((v) => {
            setVal(offset, v, true);
            offset += typedArray.BYTES_PER_ELEMENT;
        });
    }
    return data;
}

function encodeValue(val) {
    if (val === undefined) {
        return Uint8Array.of(VAL_TYPE_UNDEFINED);
    } else if (val === null) {
        return Uint8Array.of(VAL_TYPE_NULL);
    } else if (val === true) {
        return Uint8Array.of(VAL_TYPE_TRUE);
    } else if (val === false) {
        return Uint8Array.of(VAL_TYPE_FALSE);
    } else if (typeof val === 'number' && isFinite(val)) {
        return encodeNumber(val);
    } else if (Number.isNaN(val)) {
        return Uint8Array.of(VAL_TYPE_NAN);
    } else if (val === Number.POSITIVE_INFINITY) {
        return Uint8Array.of(VAL_TYPE_POSITIVE_INFINITY);
    } else if (val === Number.NEGATIVE_INFINITY) {
        return Uint8Array.of(VAL_TYPE_NEGATIVE_INFINITY);
    } else if (typeof val === 'string') {
        return encodeValueWithString(VAL_TYPE_STRING, val);
    } else if (typeof val === 'symbol') {
        return encodeValueWithString(VAL_TYPE_SYMBOL, val);
    } else if (val instanceof Date) {
        const msData = encodeNumber(val.getTime());
        const data = new Uint8Array(1 + msData.byteLength);
        const dataView = new DataView(data.buffer);
        dataView.setInt8(0, VAL_TYPE_DATE);
        data.set(msData, 1);
        return data;
    } else if (val instanceof Array) {
        const len = val.length;
        const lenData = encodeNumber(len);
        const encodedElements = val.map(encodeValue);
        const dataLen = 1 + lenData.byteLength + encodedElements.reduce((acc, v) => acc + v.byteLength, 0);
        const data = new Uint8Array(dataLen);
        const dataView = new DataView(data.buffer);
        dataView.setInt8(0, VAL_TYPE_ARRAY);
        data.set(lenData, 1);
        encodedElements.reduce((offset, v) => {
            data.set(v, offset);
            return offset + v.byteLength;
        }, 1 + lenData.byteLength);
        return data;
    } else if (val instanceof Int8Array) {
        const lenData = encodeNumber(val.byteLength);
        const data = new Uint8Array(1 + lenData.byteLength + val.byteLength);
        data[0] = VAL_TYPE_INT8ARRAY;
        data.set(lenData, 1);
        data.set(val, 1 + lenData.byteLength);
        return data;
    } else if (val instanceof Uint8Array || val instanceof Uint8ClampedArray) {
        const lenData = encodeNumber(val.byteLength);
        const data = new Uint8Array(1 + lenData.byteLength + val.byteLength);
        data[0] = VAL_TYPE_UINT8ARRAY;
        data.set(lenData, 1);
        data.set(val, 1 + lenData.byteLength);
        return data;
    } else if (val instanceof Int16Array) {
        return encodeTypedArray(VAL_TYPE_INT16ARRAY, val, DataView.prototype.setInt16);
    } else if (val instanceof Uint16Array) {
        return encodeTypedArray(VAL_TYPE_UINT16ARRAY, val, DataView.prototype.setUint16);
    } else if (val instanceof Int32Array) {
        return encodeTypedArray(VAL_TYPE_INT32ARRAY, val, DataView.prototype.setInt32);
    } else if (val instanceof Uint32Array) {
        return encodeTypedArray(VAL_TYPE_UINT32ARRAY, val, DataView.prototype.setUint32);
    } else if (val instanceof Float32Array) {
        return encodeTypedArray(VAL_TYPE_FLOAT32ARRAY, val, DataView.prototype.setFloat32);
    } else if (val instanceof Float64Array) {
        return encodeTypedArray(VAL_TYPE_FLOAT64ARRAY, val, DataView.prototype.setFloat64);
    } else if (val instanceof JSONValue) {
        const obj = val.value;
        const keys = Object.keys(obj);
        const lenData = encodeNumber(keys.length);
        const keyDataList = [];
        const keyDataLen = 0;
        const valDataList = [];
        const valDataLen = 0;
        for (let key in keys) {
            let keyData = encodeValue(key)
            keyDataList.push(keyData);
            keyDataLen += keyData.byteLength;
            let valData = encodeValue(obj[key]);
            valDataList.push(valData);
            valDataLen += valData.byteLength;
        }
        const data = new Uint8Array(1 + lenData.byteLength + keyDataLen + valDataLen);
        data[0] = VAL_TYPE_JSON;
        let offset = 1;
        data.set(lenData, offset);
        offset += lenData.byteLength;
        for (let i = 0; i < keyDataList.length; ++i) {
            data.set(keyDataList[i], offset);
            offset += keyDataList[i].byteLength;
            data.set(valDataList[i], offset);
            offset += valDataList[i].byteLength;
        }
        return data;
    } else {
        const classId = findClassId(val);
        const objectId = findObjectId(val);
        const classIdData = encodeNumber(classId);
        const objectIdData = encodeNumber(objectId);
        const data = new Uint8Array(1 + classIdData.byteLength + objectIdData.byteLength);
        data[0] = VAL_TYPE_OBJECT;
        let offset = 1;
        data.set(classIdData, offset);
        offset += classIdData.byteLength;
        data.set(objectIdData, offset);
        return data;
    }
}

function encodeValueList(futureId, vals) {
    const encodedValues = vals.map(encodeValue);
    const dataLen = 4 + encodedValues.reduce((acc, v) => acc + v.byteLength, 0);
    const data = new Uint8Array(dataLen);
    const dataView = new DataView(data.buffer);
    dataView.setUint32(0, futureId, true);
    encodedValues.reduce((offset, v) => {
        data.set(v, offset);
        return offset + v.byteLength;
    }, 4);
    return data;
}

export function callAction(name, ...args) {
    webSocket.send(JSON.stringify([name].concat(args)));
}

export function notifyValues(futureId, ...vals) {
    if (futureId) {
        const sendData = encodeValueList(futureId, vals);
        webSocket.send(sendData.buffer);
    }
}

export function logDebugMessage(msg) {
    callAction('logDebugMessage', msg);
}

export function notifyException(exception) {
    callAction("notifyException", exception.toString(), exception.stack);
}

window.addEventListener('error', (e) => {
    if (!e.error) {
        return;
    }

    console.log('Unhandled exception: ' + e.error.toString());
    console.log('stacktrace:\n' + e.error.stack);
    try {
        if (webSocket) {
            notifyException(e.error);
        }
    } catch (err) {
        console.log('notifyException failed: ' + err.toString());
        console.log('stacktrace:\n' + err.stack);
    }
});

/**
 * Graviton binary commands
 */

const binaryCommands = [];

export function registerBinaryCommand(commandId, func) {
    binaryCommands[commandId] = func;
}

function dispatchBinaryMessage(abuf) {
    const ds = new DataReadStream(abuf);
    while (ds.hasData()) {
        const commandIndex = ds.getUint16();
        const func = binaryCommands[commandIndex];
        if (func) {
            try {
                func(ds);
            } catch (e) {
                notifyException(e);
            }
        } else {
            notifyException(new Error('Invalid binary command index: ' + commandIndex));
        }
    }
}

/**
 * funcall & make
 */

export function funcall(calleeName, args) {
    const [f, self] = calleeName.split(".").reduce((acc, name) => {
        const [obj, self] = acc;
        return [obj[name], obj];
    }, [window, undefined]);
    return f.apply(self, args);
}

export function makeJSObject(className, args) {
    const klass = window[className];
    if (!klass) {
        throw `Class not found: ${className}`;
    }
    return new klass(...args);
}

export function extractJSObjectProperties(obj, props) {
    return props.map(prop => obj[prop]);
}

/**
 * animation frame
 */

let animationFrameServerCallback = undefined;
const animationFrameCallbacks = [];

function handleAnimationFrame(timestamp) {
    if (animationFrameServerCallback) {
        animationFrameServerCallback(timestamp);
        animationFrameServerCallback = undefined;
    }

    animationFrameCallbacks.forEach((callback) => {
        callback(timestamp);
    });

    window.requestAnimationFrame(handleAnimationFrame);
}

export function requestAnimationFrameServerCallback(callback) {
    animationFrameServerCallback = callback;
}

export function registerAnimationFrameCallback(callback) {
    animationFrameCallbacks.push(callback);
}

export function unregisterAnimationFrameCallback(callback) {
    const i = animationFrameCallbacks.findIndex((x) => x === callback);
    if (i < 0) {
        return;
    }
    animationFrameCallbacks.splice(i, 1);
}

/**
 * Open child window
 */
let draggingWindow = undefined;
let childWindowOffsetX = 0;
let childWindowOffsetY = 0;
let windowPositionX = window.innerWidth / 3;
let windowPositionY = window.innerHeight / 3;

function activateIframeWindow(win) {
    Array.from(document.querySelectorAll('.grv-window')).forEach((w) => {
        inactivateIframeWindow(w);
        w.style.zIndex = 'auto';
    });
    win.classList.remove('grv-window-inactive');
    win.classList.add('grv-window-active');
    win.style.zIndex = 100;
    win.querySelector('.grv-window-iframe').contentWindow.focus();
}

function inactivateIframeWindow(win) {
    win.classList.remove('grv-window-active');
    win.classList.add('grv-window-inactive');
}

function computeWindowPosition(width, height) {
    if (document.body.clientWidth <= (windowPositionX + width)) {
        windowPositionX = 0;
    }
    if (document.body.clientHeight <= (windowPositionY + height)) {
        windowPositionY = 0;
    }
    return [windowPositionX, windowPositionY];
}

function updateNextWindowPosition(delta) {
    windowPositionX += delta;
    windowPositionY += delta;
}

function openWindowIframe(path, width, height, resizable) {
    const win = document.createElement('div');
    win.classList.add('grv-window');
    win.style.visibility = 'hidden';
    const winTitlebar = document.createElement('div');
    winTitlebar.classList.add('grv-window-titlebar');
    win.appendChild(winTitlebar);
    const winTitle = document.createElement('div');
    winTitle.innerText = 'Untitled';  // Dummy text to compute the height of the titlebar.
    winTitle.classList.add('grv-window-title');
    winTitlebar.appendChild(winTitle);
    const closeButton = document.createElement('div');
    closeButton.classList.add('grv-window-close');
    winTitlebar.appendChild(closeButton);
    const iframe = document.createElement('iframe');
    iframe.classList.add('grv-window-iframe');
    if (resizable) {
        iframe.classList.add('grv-window-iframe-resizable');
    } else {
        iframe.classList.add('grv-window-iframe-fixed');
    }
    iframe.style.width = width || 800;
    iframe.style.height = height || 600;
    win.appendChild(iframe);
    document.body.appendChild(win);

    const rect = win.getBoundingClientRect();
    const [winX, winY] = computeWindowPosition(rect.width, rect.height);
    win.style.left = `${winX}px`;
    win.style.top = `${winY}px`;
    updateNextWindowPosition(winTitlebar.getBoundingClientRect().height);

    winTitle.addEventListener('mousedown', startMoveWindow);
    winTitle.addEventListener('touchstart', startMoveWindow);
    closeButton.addEventListener('click', (e) => {
        if (e.button === 0) {
            document.body.removeChild(win);
        }
    });
    iframe.addEventListener('mouseenter', endMoveWindow);
    iframe.addEventListener('load', () => {
        iframe.contentWindow.grvCloseThisWindow = () => {
            document.body.removeChild(win);
        };

        iframe.contentWindow.addEventListener('focus', (e) => {
            activateIframeWindow(win);
        });
        iframe.contentWindow.addEventListener('blur', (e) => {
            inactivateIframeWindow(win);
        });
        iframe.contentWindow.addEventListener('resize', (e) => {
            winTitlebar.style.width = `${iframe.getBoundingClientRect().width}px`;
        });

        winTitle.innerText = iframe.contentDocument.title;

        const docTitle = iframe.contentDocument.querySelector('title');
        if (!docTitle) {
            return;
        }
        const observer = new MutationObserver((mutationList, observer) => {
            winTitle.innerText = iframe.contentDocument.title;
        });
        observer.observe(docTitle, { attributes: false, childList: true, subtree: false });
        win.style.visibility = 'visible';
    });

    iframe.src = path;
    activateIframeWindow(win);
}

export function openWindow(path, width, height, resizable, useIframe) {
    if (useIframe) {
        openWindowIframe(path, width, height, resizable);
    } else {
        const opts = [`resizable=${resizable ? 'yes' : 'no'}`];
        if (width) {
            opts.push(`width=${width}`);
        }
        if (height) {
            opts.push(`height=${height}`);
        }
        window.open(path, '_blank', opts.join(','));
    }
}

function moveWindow(event) {
    event.preventDefault();

    if (!draggingWindow) {
        endMoveWindow(event);
        return;
    }

    draggingWindow.style.left = `${event.clientX - childWindowOffsetX}px`;
    draggingWindow.style.top = `${event.clientY - childWindowOffsetY}px`;
}

function endMoveWindow(event) {
    event.preventDefault();

    const layer = document.getElementById('grv-window-move-layer');
    if (!layer) {
        return;
    }
    document.body.removeChild(layer);
    draggingWindow = undefined;
}

function startMoveWindow(event) {
    if (event.button !== 0) {
        return;
    }

    const win = event.target.parentElement.parentElement;
    if (!win) {
        return;
    }

    event.preventDefault();

    activateIframeWindow(win);

    const rect = win.getBoundingClientRect();
    childWindowOffsetX = event.clientX - rect.left;
    childWindowOffsetY = event.clientY - rect.top;
    draggingWindow = win;

    const layer = document.createElement('div');
    layer.id = 'grv-window-move-layer';
    document.body.appendChild(layer);
    layer.addEventListener('mousemove', moveWindow);
    layer.addEventListener('mouseup', endMoveWindow);
    layer.addEventListener('mouseleave', endMoveWindow);
    layer.addEventListener('touchmove', moveWindow);
    layer.addEventListener('touchend', endMoveWindow);
    layer.addEventListener('touchcancel', endMoveWindow);
}

window.addEventListener('load', () => {
    window.requestAnimationFrame(handleAnimationFrame);
});
