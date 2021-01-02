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
        let containerId = `container${objectFitContainerNextId++}`;
        let containerWidthVar = `--grut-${containerId}-width`;
        let containerHeightVar = `--grut-${containerId}-height`;
        let handler = () => {
            let rect = containerElement.getBoundingClientRect();
            containerElement.style.setProperty(containerWidthVar, rect.width);
            containerElement.style.setProperty(containerHeightVar, rect.height);
        };
        containerData = {
            'width-var': containerWidthVar,
            'height-var': containerHeightVar,
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
            element.style.transform = `translate(-50%, -50%) scale(min(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-object-fit-cover':
            element.style.transform = `translate(-50%, -50%) scale(max(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar})))`;
            break;
        case 'grut-object-fit-fill':
            element.style.transform = `translate(-50%, -50%) scale(var(${containerData['width-var']}) / var(${objectWidthVar}), var(${containerData['height-var']}) / var(${objectHeightVar}))`;
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