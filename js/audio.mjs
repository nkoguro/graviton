export let audioContext = new AudioContext();
export let audioChannels = [];

function initAudioChannels() {
    for (let i = 0; i < 16; ++i) {
        audioChannels[i] = {
            lastPlaySec: 0
        };
    }
}

window.addEventListener('load', initAudioChannels);

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
        window.addEventListener(eventName, resumeAudioContext);
    });
}

window.addEventListener('load', registerAudioContextResumeHandler);
