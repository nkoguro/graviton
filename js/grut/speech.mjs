'use strict';

export function speak(text, lang, voiceName) {
    const synth = window.speechSynthesis;
    const uttr = new SpeechSynthesisUtterance(text);
    if (voiceName || lang) {
        for (const voice of synth.getVoices()) {
            if (lang && voice.lang !== lang) {
                continue;
            }
            if (voiceName && voice.name !== voiceName) {
                continue;
            }
            uttr.lang = lang;
            uttr.voice = voice;
            break;
        }
    }
    synth.speak(uttr);
}

function isSafari() {
    const userAgent = window.navigator.userAgent;
    return userAgent.includes("Safari") && !userAgent.includes('Chrome');
}

export function fetchAllVoices(proc, isWait) {
    const synth = window.speechSynthesis;
    const conv = (voices) => Array.from(voices).map(voice => [voice.default, voice.lang, voice.localService, voice.name, voice.voiceURI]);
    const voices = synth.getVoices();
    if (voices.length > 0 || isSafari() || !isWait) {
        proc(conv(voices));
    } else {
        const handler = (event) => {
            const voices = synth.getVoices();
            if (voices.length > 0) {
                proc(conv(voices));
                synth.removeEventListener("voiceschanged", handler);
            }
        };
        synth.addEventListener("voiceschanged", handler);
    }
}
