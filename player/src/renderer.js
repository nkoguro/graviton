'use strict';

const {ipcRenderer} = require('electron');

function init() {
    window.isElectron = true;
    window.closePlayer = () => {
        ipcRenderer.send('closePlayer');
    };
}

init();
