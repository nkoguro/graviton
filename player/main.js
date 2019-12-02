'use strict';

const fs = require('fs');
const path = require('path');
const {app, BrowserWindow, Menu, ipcMain} = require('electron');

var config = null;

for (var i = 0; i < process.argv.length; ++i) {
    if (process.argv[i] === '--config') {
        let configFile = process.argv[++i];
        config = JSON.parse(fs.readFileSync(configFile, 'utf-8'));
        fs.unlinkSync(configFile);
    }
}

if (!config) {
    console.log('No config specified.');;
    app.exit(1);
}

var win = null;

function createWindow() {
    let windowInitiallyShown = config['open-dev-tools'] ? true : false;
    win = new BrowserWindow({
        width: config['width'],
        height: config['height'],
        backgroundColor: config['background-color'],
        resizable: false,
        show: windowInitiallyShown,
        webPreferences: {
            preload: path.join(__dirname, 'renderer.js'),
            enableRemoteModule: false,
            nodeIntegration: false
        }
    });
    if (config['open-dev-tools']) {
        win.openDevTools();
    }
    Menu.setApplicationMenu(null);
    win.loadURL(config['url']);
}

ipcMain.on('showWindow', (event, arg) => {
    if (win) {
        win.show();
    }
});

ipcMain.on('closePlayer', (event, arg) => {
    app.exit(0);
});

app.on('ready', createWindow);
