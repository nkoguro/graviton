const eventHandlerTable = new Map();

function extractEventValues(event, props) {
    const vals = [];
    props.forEach((propSpec) => {
        var v = event;
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
