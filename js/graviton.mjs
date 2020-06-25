'use strict';

let webSocket;

function connectServer() {
    webSocket = new WebSocket('ws://' + location.host + '/s');
    webSocket.binaryType = 'arraybuffer';
    webSocket.onopen = () => {
        console.log('opened');
        callAction("startApplication");
    };
    webSocket.onclose = () => {
        console.log('closed');
        webSocket = null;
        window.close();
    };
    webSocket.onmessage = (event) => {
        try {
            if (typeof event.data === 'string') {
                notifyException("Can't handle string data: " + event.data);
            } else {
                dispatchBinaryMessage(event.data);
            }
        } catch (e) {
            notifyException(e.toString());
        }
    };
}

window.addEventListener('load', connectServer);

export function closeConnection() {
    if (webSocket) {
        webSocket.close(1000);
    }
}

let enumTable = {};
let objectRefTable = {};

export class ObjectRef {
    constructor(proxyId) {
        this.proxyId = proxyId;
        this.value = null;
    }

    static lookup(proxyId) {
        let obj = objectRefTable[proxyId];
        if (obj) {
            return obj;
        }

        obj = new ObjectRef(proxyId);
        objectRefTable[proxyId] = obj;
        return obj;
    }

    invalidate() {
        this.value = null;
        this.proxyId = null;
        objectRefTable[this.proxyId] = null;
    }
}

export function registerEnum(enumName, vals) {
    enumTable[enumName] = vals;
}

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
        let str = this.getString();
        return JSON.parse(str)[0];
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

    getObjectRef() {
        let index = this.getUint32();
        return ObjectRef.lookup(index);
    }

    getObjectRefValue() {
        let objRef = this.getObjectRef();
        return objRef.value;
    }

    getEnum(name) {
        let index = this.getUint8();
        return enumTable[name][index];
    }
}

function encodeValues(futureId, vals) {
    if (vals.length >= 256) {
        throw 'Too many values to server';
    }

    let encodeDataLen = 4;
    let jsonVals = [];
    let binaryDataMap = new Map();
    vals.forEach((v, i) => {
        if (v instanceof Uint8Array || v instanceof Uint8ClampedArray) {
            jsonVals.push(null);
            encodeDataLen += (1 + 4 + v.length);
            binaryDataMap.set(i, v);
        } else {
            jsonVals.push(v);
        }
    });
    let textEncoder = new TextEncoder();
    let jsonData = textEncoder.encode(JSON.stringify(jsonVals));
    encodeDataLen += (4 + jsonData.length);

    let encodeData = new Uint8Array(encodeDataLen);
    let encodeDataView = new DataView(encodeData.buffer);
    let i = 0;
    encodeDataView.setUint32(i, futureId, true);
    i += 4;
    encodeDataView.setUint32(i, jsonData.length, true);
    i += 4;
    encodeData.set(jsonData, i);
    i += jsonData.length;
    binaryDataMap.forEach((data, dataIndex) => {
        encodeDataView.setUint8(i, dataIndex);
        i += 1;
        encodeDataView.setUint32(i, data.length, true);
        i += 4;
        encodeData.set(data, i);
        i += data.length;
    });

    return encodeData;
}


export function callAction(name, ...args) {
    webSocket.send(JSON.stringify([name].concat(args)));
}

export function notifyValues(futureId, vals) {
    if (futureId) {
        let sendData = encodeValues(futureId, vals);
        webSocket.send(sendData.buffer);
    }
}

export function notifyException(exception) {
    callAction("notifyException", exception.toString());
}

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
                notifyException(e.toString());
            }
        } else {
            notifyException('Invalid binary command index: ' + commandIndex);
        }
    }
}
