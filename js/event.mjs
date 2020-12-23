let eventHandlerTable = new Map();

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

export function registerEventHandler(obj, type, props, callback) {
    let eventHandler = (e) => {
        let args = extractEventValues(e, props);
        callback(...args);
    };
    eventHandlerTable.set(callback, eventHandler);
    obj.addEventListener(type, eventHandler);
}

export function unregisterEventHandler(obj, type, callback) {
    let eventHandler = eventHandlerTable.get(callback);
    if (eventHandler) {
        obj.removeEventListener(type, eventHandler);
        eventHandlerTable.delete(callback);
    }
}
