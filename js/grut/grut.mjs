/* global ResizeObserver */
'use strict';

/**
 * grut-object-fit-* class handling
 */

const objectFitContainerDataTable = new Map();
const objectFitDataTable = new Map();
let objectFitContainerNextId = 0;
let objectFitNextId = 0;
const grutSpecialClassList = ['grut-contain', 'grut-cover', 'grut-fill', 'grut-none'];

const resizeObserver = new ResizeObserver(entries => {
    for (let entry of entries) {
        const e = entry.target;
        const containerData = objectFitContainerDataTable.get(e.dataset.grutObjectFitContainerId);
        if (containerData) {
            containerData.handler();
        }

        const objectFitData = objectFitDataTable.get(e.dataset.grutObjectFitId);
        if (objectFitData) {
            objectFitData.handler();
        }
    }
});

function processGrutSpecialClass(element) {
    if (!(element instanceof HTMLElement)) {
        return;
    }

    const selector = grutSpecialClassList.map((klass) => `.${klass}`).join(',');
    Array.from(element.querySelectorAll(selector)).forEach((e) => {
        processGrutSpecialClassInner(e);
    });
}

function processGrutSpecialClassInner(element) {
    const grutSpecialClass = grutSpecialClassList.find((className) => {
        return element.classList.contains(className);
    });
    if (!grutSpecialClass) {
        return;
    }

    const containerElement = element.parentElement || document.body;
    let containerData = objectFitContainerDataTable.get(containerElement.dataset.grutObjectFitContainerId);
    if (!containerData) {
        const containerId = `container${objectFitContainerNextId++}`;
        const containerWidthVar = `--grut-${containerId}-width`;
        const containerHeightVar = `--grut-${containerId}-height`;
        const containerCenterXVar = `--grut-${containerId}-center-x`;
        const containerCenterYVar = `--grut-${containerId}-center-y`;
        const handler = () => {
            const rect = containerElement.getBoundingClientRect();
            const width = rect.width;
            // The height of document.body can be 0 on Firefox. This is a workaround for the case.
            const height = (containerElement === document.body && rect.height === 0) ? window.innerHeight : rect.height;
            const centerX = rect.left + width / 2;
            const centerY = rect.top + height / 2;
            containerElement.style.setProperty(containerWidthVar, width);
            containerElement.style.setProperty(containerHeightVar, height);
            containerElement.style.setProperty(containerCenterXVar, centerX);
            containerElement.style.setProperty(containerCenterYVar, centerY);
        };
        containerData = {
            'width-var': containerWidthVar,
            'height-var': containerHeightVar,
            'center-x-var': containerCenterXVar,
            'center-y-var': containerCenterYVar,
            'handler': handler
        };
        containerElement.dataset.grutObjectFitContainerId = containerId;
        objectFitContainerDataTable.set(containerId, containerData);
        handler();
        resizeObserver.observe(containerElement);
    }

    const objectFitId = `object${objectFitNextId++}`;
    const objectWidthVar = '--grut-object-width';
    const objectHeightVar = '--grut-object-height';
    const handler = () => {
        element.style.setProperty(objectWidthVar, element.offsetWidth);
        element.style.setProperty(objectHeightVar, element.offsetHeight);
    };
    const objectData = {
        'handler': handler
    };
    element.dataset.grutObjectFitId = objectFitId;
    objectFitDataTable.set(objectFitId, objectData);
    switch (grutSpecialClass) {
        case 'grut-contain':
            element.style.left = `var(${containerData['center-x-var']})`;
            element.style.top = `var(${containerData['center-y-var']})`;
            element.style.transform = `translate(-50%, -50%) scale(min(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-cover':
            element.style.left = `var(${containerData['center-x-var']})`;
            element.style.top = `var(${containerData['center-y-var']})`;
            element.style.transform = `translate(-50%, -50%) scale(max(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-fill':
            element.style.left = `var(${containerData['center-x-var']})`;
            element.style.top = `var(${containerData['center-y-var']})`;
            element.style.transform = `translate(-50%, -50%) scale(calc(var(${containerData['width-var']}) / var(${objectWidthVar})), calc(var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-none':
            element.style.left = `var(${containerData['center-x-var']})`;
            element.style.top = `var(${containerData['center-y-var']})`;
            break;
    }
    handler();
    resizeObserver.observe(element);
}

function grutInit() {
    processGrutSpecialClass(document.body);
}

window.addEventListener('load', grutInit);
