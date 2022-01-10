'use strict';

const eventHandlerTable = new Map();

function extractEventValues(event, props) {
    const vals = [];
    props.forEach((propSpec) => {
        let v = event;
        propSpec.forEach((key) => {
            v = v[key];
        });
        vals.push(v);
    });
    return vals;
}

export function registerEventHandler(obj, type, props, callback, useCapture) {
    const eventHandler = (e) => {
        const args = extractEventValues(e, props);
        callback(...args);
    };
    eventHandlerTable.set(callback, eventHandler);
    obj.addEventListener(type, eventHandler, useCapture);
}

export function unregisterEventHandler(obj, type, callback, useCapture) {
    const eventHandler = eventHandlerTable.get(callback);
    if (eventHandler) {
        obj.removeEventListener(type, eventHandler, useCapture);
        eventHandlerTable.delete(callback);
    }
}

export function registerOneShotEventHandler(obj, type, props, proc, useCapture) {
    const eventHandler = (e) => {
        obj.removeEventListener(type, eventHandler, useCapture);
        const vals = props ? extractEventValues(e, props) : [e];
        proc(vals);
    };
    obj.addEventListener(type, eventHandler, useCapture);
}

/**
 * animation frame
 */

let animationFrameServerCallbacks = [];
let animationFrameHandleId;

function handleAnimationFrame(timestamp) {
    animationFrameServerCallbacks.forEach((callback) => {
        callback(timestamp);
    });
    if (animationFrameServerCallbacks.length > 0) {
        animationFrameHandleId = window.requestAnimationFrame(handleAnimationFrame);
    }
}

export function requestAnimationFrameServerCallback(callback) {
    animationFrameServerCallbacks.push(callback);
    if (!animationFrameHandleId && animationFrameServerCallbacks.length > 0) {
        animationFrameHandleId = window.requestAnimationFrame(handleAnimationFrame);
    }
}

export function cancelAnimationFrameServerCallback(callback) {
    const i = animationFrameServerCallbacks.findIndex(x => x === callback);
    if (i >= 0) {
        animationFrameServerCallbacks.splice(i, 1);
    }
    if (animationFrameServerCallbacks.length === 0) {
        window.cancelAnimationFrame(animationFrameHandleId);
        animationFrameHandleId = undefined;
    }
}
