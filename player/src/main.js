'use strict';

const fs = require('fs');
const path = require('path');
const { BrowserWindow, Menu, app, ipcMain } = require('electron');

let config = null;

for (var i = 0; i < process.argv.length; ++i) {
    if (process.argv[i] === '--config') {
        let configFile = process.argv[++i];
        config = JSON.parse(fs.readFileSync(configFile, 'utf-8'));
        fs.unlinkSync(configFile);
    }
}

if (!config) {
    console.log('No config specified.');
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
        if (config.show) {
            win.setBackgroundColor(backgroundColor);
            win.show();
        }
    });

    if (config['open-dev-tools']) {
        win.openDevTools();
    }
    Menu.setApplicationMenu(null);
    win.loadURL(config.url).catch((err) => {
        app.exit(70);
    });
}

ipcMain.on('closePlayer', (event, arg) => {
    app.exit(0);
});

app.on('ready', createWindow);
