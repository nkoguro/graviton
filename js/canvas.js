function centralizeCanvas(canvas) {
    let winWidth = window.innerWidth;
    let winHeight = window.innerHeight;

    let scaleByWidth = winWidth / canvas.width;
    let scaleByHeight = winHeight / canvas.height;
    let scale = Math.min(scaleByWidth, scaleByHeight);
    let scaledWidth = canvas.width * scale;
    let scaledHeight = canvas.height * scale;
    let canvasTop = (winHeight - scaledHeight) / 2;
    let canvasLeft = (winWidth - scaledWidth) / 2;

    canvas.style.position = 'absolute';
    canvas.style.top = canvasTop + 'px';
    canvas.style.left = canvasLeft + 'px';
    canvas.style.width = scaledWidth + 'px';
    canvas.style.height = scaledHeight + 'px';
}

function obj2style(ctx, style) {
    if (typeof style === 'string') {
        return style;
    } else if (typeof style === 'object') {
        switch (style['type']) {
        case 'linear-gradient':
            let linearGradient = ctx.createLinearGradient(style['x0'], style['y0'], style['x1'], style['y1']);
            style['color-stops'].forEach((colorStop) => {
                linearGradient.addColorStop(colorStop[0], colorStop[1]);
            });
            return linearGradient;
        case 'radial-gradient':
            let radialGradient = ctx.createRadialGradient(style['x0'], style['y0'], style['r0'], style['x1'], style['y1'], style['r1']);
            style['color-stops'].forEach((colorStop) => {
                radialGradient.addColorStop(colorStop[0], colorStop[1]);
            });
            return radialGradient;
        case 'pattern':
            var image = null;
            if (style['canvas']) {
                image = getProxyObject(style['canvas']);
            } else if (style['image']) {
                image = createImageBitmap(getProxyObject(style['image']));
            } else {
                throw new Error('Neither canvas nor image specified for pattern');
            }
            let pattern = ctx.createPattern(image, style['repetition']);
            return pattern;
        default:
            throw new Error('Invalid style: ' + style);
        }
    }
    throw new Error('Invalid style: ' + style);
}

function initCanvas() {
    let canvasDiv = document.createElement('div');
    canvasDiv.id = '_on';
    document.body.appendChild(canvasDiv);

    window.addEventListener('resize', (event) => {
        let elements = document.getElementById("_on").children;
        for (let i = 0; i < elements.length; ++i) {
            if (elements[i].tagName === "CANVAS") {
                centralizeCanvas(elements[i]);
            }
        }
    });
}

registerInitializer(initCanvas);
