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
                notifyException(false, "Can't handle string data: " + event.data);
            } else {
                dispatchBinaryMessage(event.data);
            }
        } catch (e) {
            notifyException(false, e.toString());
        }
    };
}

registerInitializer(connectServer);

let proxyObjectTable = {};
let reverseProxyObjectTable = {};
let enumTable = {};

function linkProxyObject(index, obj) {
    proxyObjectTable[index] = obj;
    reverseProxyObjectTable[obj] = index;
}

function unlinkProxyObject(index) {
    let obj = proxyObjectTable[index];
    if (obj != null) {
        reverseProxyObjectTable[obj] = null;
    }
    proxyObjectTable[index] = null;
}

function getProxyObject(index) {
    return proxyObjectTable[index];
}

class DataStream {
    constructor(buf) {
        this.dataView = new DataView(buf);
        this.position = 0;
        this.textDecoder = new TextDecoder('utf-8');
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

function notifyEvent(proxyId, eventType, event) {
    webSocket.send(JSON.stringify(["notifyEvent", proxyId, eventType, event]));
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

/**
 * Event
 */

let eventHandlerTable = {};

function extractEventValues(event, props) {
    let vals = [];
    props.forEach((propSpec) => {
        var v = event;
        propSpec.forEach((key) => {
            v = v[key];
        });
        vals.push(v);
    });
    return vals;
}

function eventHandlerKey(proxyId, eventName) {
    return `${proxyId}_${eventName}`;
}

function registerEventHandler(proxyId, eventTarget, eventName, props) {
    unregisterEventHandler(proxyId, eventTarget, eventName);
    let handler = (e) => {
        notifyEvent(proxyId, eventName, extractEventValues(e, props));
    };
    eventHandlerTable[eventHandlerKey(proxyId, eventName)] = handler;
    eventTarget.addEventListener(eventName, handler);
}

function unregisterEventHandler(proxyId, eventTarget, eventName) {
    let key = eventHandlerKey(proxyId, eventName);
    let handler = eventHandlerTable[key];
    if (handler) {
        eventTarget.removeEventListener(eventName, handler);
    }
    eventHandlerTable[key] = null;
}

/**
 * Window handler
 */

window.addEventListener('load', () => {
    initializeFunctions.forEach((func) => {
        func();
    });
});
