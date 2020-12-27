'use strict';

let webSocket;

function connectServer() {
    webSocket = new WebSocket('ws://' + location.host + '/_s');
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

let classIdTable = new Map();
let objectTable = new Map();
let objectIdTable = new Map();
let objectNextId = 0;

function lookupObject(objectId) {
    let obj = objectTable.get(objectId);
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
    let objectId = objectIdTable.get(obj);
    if (objectId === undefined) {
        do {
            objectId = objectNextId++;
        } while (objectTable.get(objectId));
        objectTable.set(objectId, obj);
        objectIdTable.set(obj, objectId);
    }
    return objectId;
}

export function freeObjectId(objectId) {
    let obj = objectTable.get(objectId);
    objectIdTable.delete(obj);
    objectTable.delete(objectId);
    if (objectId < objectNextId) {
        objectNextId = objectId;
    }
}

let enumTable = new Map();

export function registerEnum(enumName, vals) {
    enumTable.set(enumName, vals);
}

function extractValueWithProperties(obj, props) {
    if (props.length === 0) {
        return obj;
    }

    let val = obj[props[0]];
    let rest = props.slice(1);
    if (Number.isInteger(val.length)) {
        return Array.from(val).map((v) => extractPropertyValue(v, rest));
    } else {
        return extractPropertyValue(val, rest);
    }
}

function convertObjectToArray(obj, arrayRule) {
    let result = [];
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
    let result = {};
    Object.keys(objectRule).forEach((k) => {
        let rule = objectRule[k];
        if (rule === true) {
            result[k] = obj[k];
        } else if (typeof rule === 'string') {
            result[k] = obj[rule];
        } else if (rule instanceof Array) {
            let [elementKey, elementRule] = rule;
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

let procedureTable = new Map();

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

    getString() {
        let data = this.getUint8Array();
        return this.textDecoder.decode(data);
    }

    getJson() {
        let len = this.getAny();
        let obj = {};
        for (let i = 0; i < len; ++i) {
            let key = this.getAny();
            let val = this.getAny();
            obj[key] = val;
        }
        return obj;
    }

    getInt8Array() {
        let len = this.getAny();
        let data = new Int8Array(this.dataView.buffer, this.position, len);
        this.position += len;
        return data;
    }

    getUint8Array() {
        let len = this.getAny();
        let data = new Uint8Array(this.dataView.buffer, this.position, len);
        this.position += len;
        return data;
    }

    decodeTypedArray(getter, typedArrayClass) {
        let len = this.getAny();
        if (IS_LITTLE_ENDIAN) {
            let data = new typedArrayClass(this.dataView.buffer, this.position, len);
            this.position += len * typedArray.BYTES_PER_ELEMENT;
            return data;
        } else {
            let data = new typedArrayClass(len);
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
        let objectId = this.getAny();
        return lookupObject(objectId);
    }

    getEnum(name) {
        let index = this.getUint8();
        return enumTable.get(name)[index];
    }

    getSymbol() {
        return Symbol(this.getString());
    }

    getDate() {
        let ms = this.getAny();
        return new Date(ms);
    }

    getArray() {
        let len = this.getAny();
        let array = [];
        for (let i = 0; i < len; ++i) {
            array.push(this.getAny());
        }
        return array;
    }

    getProcedure() {
        let futureId = this.getUint32();
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
        let valType = this.getInt8();
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
    let textEncoder = new TextEncoder();
    let strData = textEncoder.encode(str);
    let strLen = strData.byteLength;
    let strLenData = encodeNumber(strLen);
    let data = new Uint8Array(1 + strLenData.byteLength + strData.byteLength);
    let dataView = new DataView(data.buffer);
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
                let data = new Uint8Array(1 + 1);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT8);
                dataView.setUint8(1, v);
                return data;
            } else if (v <= 0xffff) {
                // Uint16
                let data = new Uint8Array(1 + 2);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT16);
                dataView.setUint16(1, v, true);
                return data;
            } else if (v <= 0xffffffff) {
                // Uint32
                let data = new Uint8Array(1 + 4);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_UINT32);
                dataView.setUint32(1, v, true);
                return data;
            }
            // fallback to float64
        } else {
            if (-0x80 <= v) {
                // Int8
                let data = new Uint8Array(1 + 1);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT8);
                dataView.setInt8(1, v);
                return data;
            } else if (-0x8000 <= v) {
                // Int16
                let data = new Uint8Array(1 + 2);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT16);
                dataView.setInt16(1, v, true);
                return data;
            } else if (-0x80000000 <= v) {
                // Int32
                let data = new Uint8Array(1 + 4);
                let dataView = new DataView(data.buffer);
                dataView.setInt8(0, VAL_TYPE_INT32);
                dataView.setInt32(1, v, true);
                return data;
            }
            // fallback to float64
        }
    }
    // Float64
    let data = new Uint8Array(1 + 8);
    let dataView = new DataView(data.buffer);
    dataView.setInt8(0, VAL_TYPE_FLOAT64);
    dataView.setFloat64(1, v, true);
    return data;
}

function encodeTypedArray(valType, typedArray, filler) {
    let lenData = encodeNumber(typedArray.byteLength);
    let data = new Uint8Array(1 + lenData.byteLength + typedArray.byteLength);
    let dataView = new DataView(data.buffer);
    dataView.setInt8(0, valType);
    data.set(lenData, 1);

    let offset = 1 + lenData.byteLength;
    if (IS_LITTLE_ENDIAN) {
        data.set(typedArray, offset);
    } else {
        let setVal = filler.bind(dataView);
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
        let msData = encodeNumber(val.getTime());
        let data = new Uint8Array(1 + msData.byteLength);
        let dataView = new DataView(data.buffer);
        dataView.setInt8(0, VAL_TYPE_DATE);
        data.set(msData, 1);
        return data;
    } else if (val instanceof Array) {
        let len = val.length;
        let lenData = encodeNumber(len);
        let encodedElements = val.map(encodeValue);
        let dataLen = 1 + lenData.byteLength + encodedElements.reduce((acc, v) => acc + v.byteLength, 0);
        let data = new Uint8Array(dataLen);
        let dataView = new DataView(data.buffer);
        dataView.setInt8(0, VAL_TYPE_ARRAY);
        data.set(lenData, 1);
        encodedElements.reduce((offset, v) => {
            data.set(v, offset);
            return offset + v.byteLength;
        }, 1 + lenData.byteLength);
        return data;
    } else if (val instanceof Int8Array) {
        let lenData = encodeNumber(val.byteLength);
        let data = new Uint8Array(1 + lenData.byteLength + val.byteLength);
        data[0] = VAL_TYPE_INT8ARRAY;
        data.set(lenData, 1);
        data.set(val, 1 + lenData.byteLength);
        return data;
    } else if (val instanceof Uint8Array || val instanceof Uint8ClampedArray) {
        let lenData = encodeNumber(val.byteLength);
        let data = new Uint8Array(1 + lenData.byteLength + val.byteLength);
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
        let obj = val.value;
        let keys = Object.keys(obj);
        let lenData = encodeNumber(keys.length);
        let keyDataList = [];
        let keyDataLen = 0;
        let valDataList = [];
        let valDataLen = 0;
        for (let key in keys) {
            let keyData = encodeValue(key)
            keyDataList.push(keyData);
            keyDataLen += keyData.byteLength;
            let valData = encodeValue(obj[key]);
            valDataList.push(valData);
            valDataLen += valData.byteLength;
        }
        let data = new Uint8Array(1 + lenData.byteLength + keyDataLen + valDataLen);
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
        let classId = findClassId(val);
        let objectId = findObjectId(val);
        let classIdData = encodeNumber(classId);
        let objectIdData = encodeNumber(objectId);
        let data = new Uint8Array(1 + classIdData.byteLength + objectIdData.byteLength);
        data[0] = VAL_TYPE_OBJECT;
        let offset = 1;
        data.set(classIdData, offset);
        offset += classIdData.byteLength;
        data.set(objectIdData, offset);
        return data;
    }
}

function encodeValueList(futureId, vals) {
    let encodedValues = vals.map(encodeValue);
    let dataLen = 4 + encodedValues.reduce((acc, v) => acc + v.byteLength, 0);
    let data = new Uint8Array(dataLen);
    let dataView = new DataView(data.buffer);
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
        let sendData = encodeValueList(futureId, vals);
        webSocket.send(sendData.buffer);
    }
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

let binaryCommands = [];

export function registerBinaryCommand(commandId, func) {
    binaryCommands[commandId] = func;
}

function dispatchBinaryMessage(abuf) {
    let ds = new DataReadStream(abuf);
    while (ds.hasData()) {
        let commandIndex = ds.getUint16();
        let func = binaryCommands[commandIndex];
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
 * grv-object-fit-* class handling
 */

const objectFitContainerDataTable = new Map();
const objectFitDataTable = new Map();
let objectFitContainerNextId = 0;
let objectFitNextId = 0;
const grvSpecialClassList = ['grv-object-fit-contain', 'grv-object-fit-cover', 'grv-object-fit-fill'];

const resizeObserver = new ResizeObserver(entries => {
    for (let entry of entries) {
        let e = entry.target;
        let containerData = objectFitContainerDataTable.get(e.dataset.grvObjectFitContainerId);
        if (containerData) {
            containerData.handler();
        }

        let objectFitData = objectFitDataTable.get(e.dataset.grvObjectFitId);
        if (objectFitData) {
            objectFitData.handler();
        }
    }
});

export function processGrvSpecialClass(element) {
    if (!(element instanceof HTMLElement)) {
        return;
    }

    let selector = grvSpecialClassList.map((klass) => `.${klass}`).join(',');
    Array.from(element.querySelectorAll(selector)).forEach((e) => {
        processGrvSpecialClassInner(e);
    });
}

function processGrvSpecialClassInner(element) {
    let grvSpecialClass = grvSpecialClassList.find((className) => {
        return element.classList.contains(className);
    });
    if (!grvSpecialClass) {
        return;
    }

    let containerElement = undefined;
    for (let e = element.parentElement; e; e = e.parentElement) {
        let computedStyle = window.getComputedStyle(e);
        if (computedStyle.position !== 'static') {
            containerElement = e;
            break;
        }
    }
    if (!containerElement) {
        containerElement = document.body;
    }
    let containerData = objectFitContainerDataTable.get(containerElement.dataset.grvObjectFitContainerId);
    if (!containerData) {
        let containerId = `container${objectFitContainerNextId++}`;
        let containerWidthVar = `--grv-${containerId}-width`;
        let containerHeightVar = `--grv-${containerId}-height`;
        let handler = () => {
            let rect = containerElement.getBoundingClientRect();
            containerElement.style.setProperty(containerWidthVar, rect.width);
            containerElement.style.setProperty(containerHeightVar, rect.height);
        };
        containerData = {
            'width-var': containerWidthVar,
            'height-var': containerHeightVar,
            'handler': handler
        }
        containerElement.dataset.grvObjectFitContainerId = containerId;
        objectFitContainerDataTable.set(containerId, containerData);
        handler();
        resizeObserver.observe(containerElement);
    }

    let objectFitId = `object${objectFitNextId++}`;
    let objectWidthVar = '--grv-object-width';
    let objectHeightVar = '--grv-object-height';
    let handler = () => {
        element.style.setProperty(objectWidthVar, element.offsetWidth);
        element.style.setProperty(objectHeightVar, element.offsetHeight);
    }
    let objectData = {
        'handler': handler
    };
    element.dataset.grvObjectFitId = objectFitId;
    objectFitDataTable.set(objectFitId, objectData);
    switch (grvSpecialClass) {
        case 'grv-object-fit-contain':
            element.style.transform = `translate(-50%, -50%) scale(min(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grv-object-fit-cover':
            element.style.transform = `translate(-50%, -50%) scale(max(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grv-object-fit-fill':
            element.style.transform = `translate(-50%, -50%) scale(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar}))`;
            break;
    }
    handler();
    resizeObserver.observe(element);
}

window.addEventListener('load', () => {
    processGrvSpecialClass(document.body);
});

/**
 * funcall & make
 */

export function funcall(calleeName, args) {
    let [f, self] = calleeName.split(".").reduce((acc, name) => {
        let [obj, self] = acc;
        return [obj[name], obj];
    }, [window, undefined]);
    return f.apply(self, args);
}

export function makeJSObject(className, args) {
    let klass = window[className];
    if (!klass) {
        throw `Class not found: ${className}`;
    }
    return new klass(...args);
}

/**
 * animation frame
 */

let animationFrameServerCallback = undefined;
let animationFrameCallbacks = [];

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
    let i = animationFrameCallbacks.findIndex((x) => x === callback);
    if (i < 0) {
        return;
    }
    animationFrameCallbacks.splice(i, 1);
}

window.addEventListener('load', () => {
    window.requestAnimationFrame(handleAnimationFrame);
});
