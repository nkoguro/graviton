'use strict';

const fs = require('fs');
const path = require('path');
const { BrowserWindow, Menu, app, ipcMain } = require('electron');

let config = { visible: true };

var i = 0;
while (i < process.argv.length - 1) {
    switch (process.argv[i++]) {
        case '--width':
            config.width = Number.parseInt(process.argv[i++]);
            break;
        case '--height':
            config.height = Number.parseInt(process.argv[i++]);
            break;
        case '--use-dev-tools':
            config.useDevTools = (process.argv[i++].toLowerCase() === 'true');
            break;
        case '--visible':
            config.visible = (process.argv[i++].toLowerCase() === 'true');
            break;
        case '--resizable':
            config.resizable = (process.argv[i++].toLowerCase() === 'true');
            break;
        default:
            // Ignore undefined options.
            break;
    }
}

config.url = process.argv[i];

if (!config.url) {
    console.log('No URL specified.');
    app.exit(1);
}

let win = null;

function createWindow() {
    const windowOption = {
        useContentSize: true,
        // BrowserWindow's actual height will be incorrect if resizable = false,
        // even though useContentSize = true. So set true here and set the
        // actual value later.
        resizable: true,
        show: false,
        webPreferences: {
            preload: path.join(__dirname, 'renderer.js'),
            enableRemoteModule: false,
            nodeIntegration: false
        }
    };
    if (config.width) {
        windowOption.width = config.width;
    }
    if (config.height) {
        windowOption.height = config.height;
    }
    win = new BrowserWindow(windowOption);
    const readyToShowPromise = new Promise(resolve => {
        win.once('ready-to-show', () => {
            resolve();
        });
    });
    const setBackgroundColorPromise = new Promise(resolve => {
        ipcMain.once('setBackgroundColor', (event, backgroundColor) => {
            resolve(backgroundColor);
        });
    });
    Promise.all([readyToShowPromise, setBackgroundColorPromise]).then((values) => {
        const [, backgroundColor] = values;
        if (config.visible) {
            win.setBackgroundColor(backgroundColor);
            win.show();
        }
    });

    if (config.useDevTools) {
        win.openDevTools();
    }
    Menu.setApplicationMenu(null);
    win.loadURL(config.url).catch((err) => {
        console.log(err);
        app.exit(70);
    });
}

ipcMain.on('closePlayer', (event, arg) => {
    app.exit(0);
});

app.on('ready', createWindow);
