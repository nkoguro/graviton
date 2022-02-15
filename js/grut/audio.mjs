'use strict';

import { audioContext } from '/_g/audio.mjs';
import { isSafari } from "/_g/graviton.mjs";

const SINE_WAVE = 1;
const SQUARE_WAVE = 2;
const SAWTOOTH_WAVE = 3;
const TRIANGLE_WAVE = 4;
const CUSTOM_WAVE = 5;

let customWave;

function fillWaveType(oscNode, spec) {
    switch (spec) {
        case SINE_WAVE:
            oscNode.type = 'sine';
            break;
        case SQUARE_WAVE:
            oscNode.type = 'square';
            break;
        case SAWTOOTH_WAVE:
            oscNode.type = 'sawtooth';
            break;
        case TRIANGLE_WAVE:
            oscNode.type = 'triangle';
            break;
        case CUSTOM_WAVE: {
            if (customWave) {
                oscNode.setPeriodicWave(customWave);
            } else {
                throw new Error('Custom wave not found');
            }
            break;
        }
        default: {
            if (spec instanceof Array) {
                const [real, imag] = spec;
                oscNode.setPeriodicWave(audioContext.createPeriodicWave(real, imag));
            } else {
                throw new Error(`Invalid wave type: ${spec}`);
            }
        }
    }
}

const VALUE_AT_TIME_PARAM = 1;
const LINEAR_RAMP_TO_VALUE_AT_TIME_PARAM = 2;
const EXPONENTIAL_RAMP_TO_VALUE_AT_TIME_PARAM = 3;
const TARGET_AT_TIME_PARAM = 4;
const VALUE_CURVE_AT_TIME_PARAM = 5;
const OSCILLATOR_PARAM = 6;

function decorateAudioParam(audioParam, specs, len) {
    const starters = [];

    if (!(specs instanceof Array)) {
        audioParam.value = specs;
        return starters;
    }

    specs.forEach((spec) => {
        const type = spec.shift();
        switch (type) {
            case VALUE_AT_TIME_PARAM: {
                const [value, relStartTime] = spec;
                starters.push((startTime) => {
                    audioParam.setValueAtTime(value, startTime + relStartTime);
                });
                break;
            }
            case LINEAR_RAMP_TO_VALUE_AT_TIME_PARAM: {
                const [value, relEndTime] = spec;
                starters.push((startTime) => {
                    audioParam.linearRampToValueAtTime(value, startTime + relEndTime);
                });
                break;
            }
            case EXPONENTIAL_RAMP_TO_VALUE_AT_TIME_PARAM: {
                const [value, relEndTime] = spec;
                starters.push((startTime) => {
                    audioParam.exponentialRampToValueAtTime(value, startTime + relEndTime);
                });
                break;
            }
            case TARGET_AT_TIME_PARAM: {
                const [value, relStartTime, timeConstant] = spec;
                starters.push((startTime) => {
                    audioParam.setTargetAtTime(value, startTime + relStartTime, timeConstant);
                });
                break;
            }
            case VALUE_CURVE_AT_TIME_PARAM: {
                const [values, relStartTime, duration] = spec;
                starters.push((startTime) => {
                    audioParam.setValueCurveAtTime(values, startTime + relStartTime, duration);
                });
                break;
            }
            case OSCILLATOR_PARAM: {
                const [freqSpec, waveSpec, detuneSpec, delay, depthSpec] = spec;
                const oscillator = new Oscillator(freqSpec, waveSpec, detuneSpec, len, delay, depthSpec);
                oscillator.node.connect(audioParam);
                starters.push(oscillator.starter);
                break;
            }
            default:
                throw new Error(`Invalid audio param decorator type: ${type}`);
        }
    });

    return starters;
}

class Oscillator {
    constructor(freqSpec, waveSpec, detuneSpec, len, delay, depthSpec) {
        const oscillator = audioContext.createOscillator();
        const gain = audioContext.createGain();
        oscillator.connect(gain);
        fillWaveType(oscillator, waveSpec);

        this.childStarters = [];
        this.childStarters.push(...decorateAudioParam(oscillator.frequency, freqSpec, len));
        this.childStarters.push(...decorateAudioParam(oscillator.detune, detuneSpec, len));
        this.childStarters.push(...decorateAudioParam(gain.gain, depthSpec, len));

        this.lengthSec = len;
        this.node = gain;
        this.starter = (startTime) => {
            this.childStarters.forEach((starter) => starter(startTime));
            oscillator.start(startTime + delay);
            oscillator.stop(startTime + len);
        };
    }
}

class OscillatorSoundlet {
    constructor(freqSpec, waveSpec, detuneSpec, len, margin, release, depthSpec, panSpec) {
        const osc = new Oscillator(freqSpec, waveSpec, detuneSpec, len + release, 0, depthSpec);
        const stereoPanner = audioContext.createStereoPanner();
        osc.node.connect(stereoPanner).connect(audioContext.destination);

        const childStarters = [];
        if (panSpec) {
            this.childStarters.push(...decorateAudioParam(stereoPanner.pan, panSpec, len + release));
        }

        this.soundLength = len + margin;
        this.soundTotalLength = len + release + margin;
        this.starter = (startTime) => {
            childStarters.forEach((starter) => starter(startTime));
            osc.starter(startTime);
        };
    }

    play(startTime) {
        this.starter(startTime);
    }
}

class ComposedSoundlet {
    constructor(soundlets) {
        this.soundlets = soundlets;
        this.soundLength = Math.max(...this.soundlets.map((s) => s.soundLength));
        this.soundTotalLength = Math.max(...this.soundlets.map((s) => s.soundTotalLength));
    }

    play(startTime) {
        this.soundlets.forEach((s) => s.play(startTime));
    }
}

let noiseBuffer = null;

class NoiseSoundlet {
    constructor(len, margin, release, depthSpec, panSpec) {
        if (!noiseBuffer) {
            noiseBuffer = audioContext.createBuffer(2, audioContext.sampleRate, audioContext.sampleRate);
            for (let i = 0; i < 2; ++i) {
                let buf = noiseBuffer.getChannelData(i);
                for (let j = 0; j < audioContext.sampleRate; ++j) {
                    buf[j] = Math.random() * 2 - 1;
                }
            }
        }

        const bufferSource = audioContext.createBufferSource();
        bufferSource.buffer = noiseBuffer;
        bufferSource.loop = true;

        const childStarters = [];
        const gain = audioContext.createGain();
        const stereoPanner = audioContext.createStereoPanner();
        bufferSource.connect(gain).connect(stereoPanner).connect(audioContext.destination);
        childStarters.push(...decorateAudioParam(gain.gain, depthSpec, len + release));
        childStarters.push(...decorateAudioParam(stereoPanner.pan, panSpec, len + release));

        this.soundLength = len + margin;
        this.soundTotalLength = len + margin + release;
        this.starter = (startTime) => {
            childStarters.forEach((starter) => starter(startTime));
            bufferSource.start(startTime);
            bufferSource.stop(startTime + len + margin);
        };
    }

    play(startTime) {
        this.starter(startTime);
    }
}

class RestSoundlet {
    constructor(len) {
        this.soundLength = len;
        this.soundTotalLength = len;
    }

    play(startTime) {
        // DO NOTHING
    }
}

const soundPlayIntervalMs = 100;
const initialWaitSec = 0.2;

class SoundTrack {
    constructor() {
        this.soundletQueue = [];
        this.releasedTime = Math.max(audioContext.currentTime, initialWaitSec);
        this.completionTime = Math.max(audioContext.currentTime, initialWaitSec);
        this.state = 'stopped';
        this.completionResolves = [];
    }

    computeNextStartTime(now) {
        return Math.max(this.releasedTime, now);
    }

    enqueue(soundlet) {
        this.soundletQueue.push(soundlet);
    }

    addCompletionResolve(resolve) {
        this.completionResolves.push(resolve);
    }

    process(now) {
        switch (this.state) {
            case 'paused':
                return false;
            case 'stopped':
                this.soundletQueue = [];
                this.invokeCompletionResolves();
                return false;
            case 'playing': {
                const rangeStartMs = audioContext.currentTime * 1000;
                const rangeEndMs = rangeStartMs + soundPlayIntervalMs * 2;
                while (this.soundletQueue.length > 0) {
                    const soundlet = this.soundletQueue[0];
                    const startTime = this.computeNextStartTime(now);
                    const startTimeMs = startTime * 1000;
                    if (startTimeMs <= rangeEndMs) {
                        soundlet.play(startTime);
                        this.releasedTime = startTime + soundlet.soundLength;
                        this.completionTime = startTime + soundlet.soundTotalLength;
                        this.soundletQueue.shift();
                    } else {
                        break;
                    }
                }

                // If the browser doesn't support AudioContext.getOutputTimestamp(), 100 ms is used as latency. 
                let latency = 0.1;
                if (!isSafari() && audioContext.getOutputTimestamp) {
                    latency = audioContext.currentTime - audioContext.getOutputTimestamp().contextTime;
                }
                if (this.soundletQueue.length === 0 && (this.completionTime + latency) < now) {
                    this.invokeCompletionResolves();
                }

                return true;
            }
            default:
                throw new Error(`Invalid state: ${this.state}`);
        }
    }

    invokeCompletionResolves() {
        this.completionResolves.forEach((resolve) => resolve());
        this.completionResolves = [];
    }
}

const OSCILLATOR_SOUND = 1;
const COMPOSED_SOUND = 2;
const NOISE_SOUND = 3;
const REST_SOUND = 5;
const SET_CUSTOM_WAVE = 6;

class SoundTrackManager {
    constructor() {
        this.soundTrackTable = new Map();
        this.noisePeriodicWave = null;
        this.timeoutId = null;
    }

    getTrack(trackName) {
        const track = this.soundTrackTable.get(trackName);
        if (track) {
            return track;
        }

        const newTrack = new SoundTrack(trackName);
        this.soundTrackTable.set(trackName, newTrack);
        return newTrack;
    }

    computeFrequencyFromNoteNum(noteNum) {
        return 440 * 2 ** ((noteNum - 69) / 12);
    }

    computeFrequencyFromNoteSpec(noteSpec) {
        if (noteSpec instanceof Array) {
            const [noteNum, vibratoFreq, delta] = noteSpec;
            return [this.computeFrequencyFromNoteNum(noteNum), vibratoFreq, delta];
        } else {
            return this.computeFrequencyFromNoteNum(noteSpec);
        }
    }

    decodeSoundData(soundData) {
        const soundType = soundData.shift();
        switch (soundType) {
            case OSCILLATOR_SOUND: {
                const [freqSpec, waveSpec, detuneSpec, len, delay, release, depthSpec, panSpec] = soundData;
                return new OscillatorSoundlet(freqSpec, waveSpec, detuneSpec, len, delay, release, depthSpec, panSpec);
            }
            case COMPOSED_SOUND: {
                return new ComposedSoundlet(soundData.map((element) => this.decodeSoundData(element)));
            }
            case NOISE_SOUND: {
                const [len, release, depthSpec, panSpec] = soundData;
                return new NoiseSoundlet(len, release, depthSpec, panSpec);
            }
            case REST_SOUND: {
                const len = soundData.shift();
                return new RestSoundlet(len);
            }
            case SET_CUSTOM_WAVE:
                const [real, imag] = soundData;
                customWave = audioContext.createPeriodicWave(real, imag);
                return false;
            default:
                throw new Error(`Invalid sound type:${soundType}`);
        }
    }


    enqueue(trackName, soundDataList) {
        const track = this.getTrack(trackName);
        soundDataList.forEach((soundData) => {
            const soundlet = this.decodeSoundData(soundData);
            if (soundlet) {
                track.enqueue(soundlet);
            }
        });
    }

    start(trackNames) {
        const now = audioContext.currentTime;
        const tracks = trackNames.map((name) => {
            const track = this.getTrack(name);
            track.state = 'playing';
            return track;
        });
        this.update(now, tracks);
        this.scheduleBackgroundUpdate();
    }

    startAll() {
        this.start(Array.from(this.soundTrackTable.keys()));
    }

    pause(trackNames) {
        trackNames.forEach((name) => {
            this.getTrack(name).state = 'paused';
        });
    }

    pauseAll() {
        this.stop(Array.from(this.soundTrackTable.keys()));
    }

    stop(trackNames) {
        trackNames.forEach((name) => {
            this.getTrack(name).state = 'stopped';
        });
    }

    stopAll() {
        this.stop(Array.from(this.soundTrackTable.keys()));
    }

    waitCompletion(trackNames, callback) {
        const promises = trackNames.map((name) => {
            const track = this.getTrack(name);
            return new Promise((resolve) => {
                track.addCompletionResolve(resolve);
            });
        });
        Promise.all(promises).then(() => {
            callback();
        });
    }

    waitAllCompletion(callback) {
        this.waitCompletion(Array.from(this.soundTrackTable.keys()), callback);
    }

    update(now, tracks) {
        const numPlaying = tracks.reduce((numPlaying, track) => {
            if (track.process(now)) {
                return numPlaying + 1;
            } else {
                return numPlaying;
            }
        }, 0);
        return numPlaying;
    }

    backgroundUpdate() {
        const allTracks = Array.from(this.soundTrackTable.values());
        const now = audioContext.currentTime;
        const numPlaying = this.update(now, allTracks);
        if (numPlaying > 0) {
            this.scheduleBackgroundUpdate();
        }
    }

    scheduleBackgroundUpdate() {
        if (this.timeoutId) {
            return;
        }

        this.timeoutId = setTimeout(() => {
            this.timeoutId = null;
            this.backgroundUpdate();
        }, soundPlayIntervalMs);
    }
}

export const soundTrackManager = new SoundTrackManager();
