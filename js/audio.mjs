/* global AudioContext */
'use strict';

export let audioContext = new AudioContext();

function registerAudioContextResumeHandler() {
    if (audioContext.state !== 'suspended') {
        return;
    }

    let eventNames = ['touchstart', 'touchend', 'mousedown', 'keydown'];
    function resumeAudioContext() {
        audioContext.resume().then(removeEvents);
    }
    function removeEvents() {
        eventNames.forEach((eventName) => {
            window.removeEventListener(eventName, resumeAudioContext);
        });
    }
    eventNames.forEach((eventName) => {
        window.addEventListener(eventName, resumeAudioContext, true);
    });
}

window.addEventListener('load', registerAudioContextResumeHandler);
