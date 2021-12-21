'use strict';

export function copyTextToClipboard(str) {
    const cb = navigator.clipboard || window.clipboard;
    if (cb) {
        cb.writeText(str);
    } else {
        const activeElement = document.activeElement;
        const textArea = document.createElement('textarea');
        textArea.textContent = str;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
        if (activeElement) {
            activeElement.focus();
        }
    }
}
