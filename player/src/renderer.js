'use strict';

const {ipcRenderer} = require('electron');

var windowShown = false;

function init() {
    window.isElectron = true;
    window.showBrowserWindow = () => {
        if (!windowShown) {
            ipcRenderer.send('showWindow');
            windowShown = true;
        }
    };
    window.closePlayer = () => {
        ipcRenderer.send('closePlayer');
    };
}

init();
