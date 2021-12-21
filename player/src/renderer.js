/* jshint browser: true */
'use strict';

const {clipboard, ipcRenderer} = require('electron');

function init() {
    window.isElectron = true;
    window.closePlayer = () => {
        ipcRenderer.send('closePlayer');
    };
    window.clipboard = clipboard;
    window.addEventListener('load', () => {
        ipcRenderer.send('setBackgroundColor', window.getComputedStyle(document.body).backgroundColor);
    });
}

init();
