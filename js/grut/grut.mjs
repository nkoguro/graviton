/**
 * grut-object-fit-* class handling
 */

const objectFitContainerDataTable = new Map();
const objectFitDataTable = new Map();
let objectFitContainerNextId = 0;
let objectFitNextId = 0;
const grutSpecialClassList = ['grut-object-fit-contain', 'grut-object-fit-cover', 'grut-object-fit-fill'];

const resizeObserver = new ResizeObserver(entries => {
    for (let entry of entries) {
        let e = entry.target;
        let containerData = objectFitContainerDataTable.get(e.dataset.grutObjectFitContainerId);
        if (containerData) {
            containerData.handler();
        }

        let objectFitData = objectFitDataTable.get(e.dataset.grutObjectFitId);
        if (objectFitData) {
            objectFitData.handler();
        }
    }
});

function processGrutSpecialClass(element) {
    if (!(element instanceof HTMLElement)) {
        return;
    }

    let selector = grutSpecialClassList.map((klass) => `.${klass}`).join(',');
    Array.from(element.querySelectorAll(selector)).forEach((e) => {
        processGrutSpecialClassInner(e);
    });
}

function processGrutSpecialClassInner(element) {
    let grutSpecialClass = grutSpecialClassList.find((className) => {
        return element.classList.contains(className);
    });
    if (!grutSpecialClass) {
        return;
    }

    let containerElement = undefined;
    for (let e = element.parentElement; e; e = e.parentElement) {
        let computedStyle = window.getComputedStyle(e);
        if (computedStyle.position !== 'static') {
            containerElement = e;
            break;
        }
    }
    if (!containerElement) {
        containerElement = document.body;
    }
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
            const height = rect.height;
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
        }
        containerElement.dataset.grutObjectFitContainerId = containerId;
        objectFitContainerDataTable.set(containerId, containerData);
        handler();
        resizeObserver.observe(containerElement);
    }

    let objectFitId = `object${objectFitNextId++}`;
    let objectWidthVar = '--grut-object-width';
    let objectHeightVar = '--grut-object-height';
    let handler = () => {
        element.style.setProperty(objectWidthVar, element.offsetWidth);
        element.style.setProperty(objectHeightVar, element.offsetHeight);
    }
    let objectData = {
        'handler': handler
    };
    element.dataset.grutObjectFitId = objectFitId;
    objectFitDataTable.set(objectFitId, objectData);
    switch (grutSpecialClass) {
        case 'grut-object-fit-contain':
            element.style.left = `var(${containerData['center-x-var']})`;
            element.style.top = `var(${containerData['center-y-var']})`;
            element.style.transform = `translate(-50%, -50%) scale(min(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-object-fit-cover':
            element.style.left = `var(${containerCenterXVar})`;
            element.style.top = `var(${containerCenterYVar})`;
            element.style.transform = `scale(max(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-object-fit-fill':
            element.style.left = `var(${containerCenterXVar})`;
            element.style.top = `var(${containerCenterYVar})`;
            element.style.transform = `scale(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar}))`;
            break;
        case 'grut-object-fit-none':
            element.style.left = `var(${containerCenterXVar})`;
            element.style.top = `var(${containerCenterYVar})`;
            break;
    }
    handler();
    resizeObserver.observe(element);
}

/**
 * Update grv-text/grv-text-edit width/height
 */

function adjustGrvTextGeometry(grvText) {
    let dataWidth = Number(grvText.dataset.width);
    if (dataWidth) {
        grvText.style.width = grvText.characterWidth * dataWidth;
    } else {
        grvText.style.width = '100%';
    }
    let dataHeight = Number(grvText.dataset.height);
    if (dataHeight) {
        grvText.style.height = grvText.lineHeight * dataHeight;
    } else {
        grvText.style.height = '100%';
    }
}


window.addEventListener('load', () => {
    document.querySelectorAll("grv-text, grv-text-edit").forEach(adjustGrvTextGeometry);
    processGrutSpecialClass(document.body);
});