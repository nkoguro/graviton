import {callAction} from '/js/graviton/graviton.mjs';

let eventHandlerTable = {};

function notifyEvent(proxyId, eventType, event) {
    callAction("notifyEvent", proxyId, eventType, event);
}

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

export function registerEventHandler(proxyId, eventTarget, eventName, props) {
    unregisterEventHandler(proxyId, eventTarget, eventName);
    let handler = (e) => {
        notifyEvent(proxyId, eventName, extractEventValues(e, props));
    };
    eventHandlerTable[eventHandlerKey(proxyId, eventName)] = handler;
    eventTarget.addEventListener(eventName, handler);
}

export function unregisterEventHandler(proxyId, eventTarget, eventName) {
    let key = eventHandlerKey(proxyId, eventName);
    let handler = eventHandlerTable[key];
    if (handler) {
        eventTarget.removeEventListener(eventName, handler);
    }
    eventHandlerTable[key] = null;
}
