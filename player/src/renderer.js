'use strict';

const {clipboard, ipcRenderer} = require('electron');

function init() {
    window.isElectron = true;
    window.closePlayer = () => {
        ipcRenderer.send('closePlayer');
    };
    window.clipboard = clipboard;
}

init();
