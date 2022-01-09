/* global HTMLSpanElement, customElements */
'use strict';

import { registerAnimationFrameCallback, logDebugMessage } from "/_g/graviton.mjs";
import { copyTextToClipboard } from "/_g/grut/clipboard.mjs";

const MAIN_CURSOR_ID = 'grv-main-cursor-id';
class AttributedCharacter {
    constructor(attr, char) {
        this.attribute = attr;
        this.character = char;
    }
}

class TextAttribute {
    constructor(cssClasses, cssStyleJson) {
        this.cssClasses = cssClasses;
        this.styleJson = cssStyleJson;
    }

    static fromStyleJson(cssStyleJson) {
        return new TextAttribute([], cssStyleJson);
    }

    static fromCssClass(cssClass) {
        return new TextAttribute([cssClass], {});
    }

    withCharacters(chars) {
        return chars.map((c) => {
            return new AttributedCharacter(this, c);
        });
    }

    withString(str) {
        return this.withCharacters(Array.from(str));
    }

    withCharacter(char) {
        return new AttributedCharacter(this, char);
    }

    concatClasses(cssClasses) {
        return new TextAttribute(this.cssClasses.concat(cssClasses), this.styleJson);
    }

    blend(attr) {
        const newCssClasses = this.cssClasses.concat(attr.cssClasses);
        const newStyleJson = Object.assign({}, this.styleJson);
        Object.keys(attr.styleJson).forEach((key) => {
            newStyleJson[key] = attr.styleJson[key];
        });
        return new TextAttribute(newCssClasses, newStyleJson);
    }

    equals(obj) {
        if (!(obj instanceof TextAttribute)) {
            return false;
        }

        if (this.cssClasses.length !== obj.cssClasses.length) {
            return false;
        }

        for (let i = 0; i < this.cssClasses.length; ++i) {
            if (this.cssClasses[i] !== obj.cssClasses[i]) {
                return false;
            }
        }

        const thisCssStyleKeys = Object.keys(this.styleJson);
        const thatCssStyleKeys = Object.keys(obj.styleJson);
        if (thisCssStyleKeys.length !== thatCssStyleKeys.length) {
            return false;
        }
        return thisCssStyleKeys.every((key) => {
            return this.styleJson[key] === obj.styleJson[key];
        });
    }

    updateElement(element) {
        element.removeAttribute('class');
        this.cssClasses.forEach((cssClass) => {
            if (cssClass.length > 0) {
                element.classList.add(cssClass);
            }
        });
        element.removeAttribute('style');
        Object.keys(this.styleJson).forEach((key) => {
            let value = this.styleJson[key];
            element.style[key] = value;
            switch (key) {
                case 'background-color':
                    element.style.setProperty(BACKGROUND_COLOR_PROPERTY, value);
                    break;
                case 'color':
                    element.style.setProperty(COLOR_PROPERTY, value);
                    break;
                default:
                    break;
            }
        });
    }
}

class TextCursor {
    constructor(textEdit) {
        this.textEdit = textEdit;
        this._column = 0;
        this._row = 0;
        this._mode = 'vertical-blink';
        this.visible = false;
    }

    get column() {
        return Math.min(this._column, this.textEdit.line().length);
    }

    get rawColumn() {
        return this._column;
    }

    get row() {
        return this._row;
    }

    set column(col) {
        if (col < 0) {
            throw new RangeError('Invalid cursor column');
        }
        this._column = col;
        this.textEdit.requestUpdateRow(this.row);
    }

    set row(row) {
        if (row < 0 || this.textEdit.rows <= row) {
            throw new RangeError('Invalid cursor row');
        }
        const prevRow = this.row;
        this._row = row;
        this.textEdit.requestUpdateRow(prevRow, this.row);
    }

    get mode() {
        return this._mode;
    }

    set mode(mode) {
        if (!CURSOR_ATTRIBUTE_TABLE[mode]) {
            throw new RangeError('Invalid cursor mode');
        }
        this._mode = mode;
        this.textEdit.requestUpdateRow(this.row);
    }

    get attribute() {
        if (this.visible) {
            return CURSOR_ATTRIBUTE_TABLE[this._mode];
        } else {
            return DEFAULT_ATTRIBUTE;
        }
    }

    atBeginningOfLine() {
        return this.column === this.textEdit.startColumnList[this.row];
    }

    atEndOfLine() {
        return this.column === this.textEdit.line().length;
    }

    inFirstLine() {
        return this.row === 0;
    }

    inLastLine() {
        return this.row === this.textEdit.rows - 1;
    }

    atBeginningOfConsole() {
        return this.inFirstLine() && this.atBeginningOfLine();
    }

    atEndOfConsole() {
        return this.inLastLine() && this.atEndOfLine();
    }

    existsAt(col, row) {
        return this.column === col && this.row === row;
    }

    show() {
        this.visible = true;
        this.textEdit.requestUpdateRow(this.row);
    }

    hide() {
        this.visible = false;
        this.textEdit.requestUpdateRow(this.row);
    }
}

class UpdateContext {
    constructor(cursor, mark, hasFocus) {
        this.cursor = cursor;
        this.mark = mark;
        this.hasFocus = hasFocus;
    }
}

class LineUpdater {
    constructor(context, offset, row, attrChars, spanIter) {
        this.context = context;
        this.offset = offset;
        this.row = row;
        this.attrChars = attrChars;
        this.spanIter = spanIter;
    }

    computeAttr(attr, col, endOfLine) {
        let computedAttr = attr;
        if (this.context.hasFocus && this.context.cursor.existsAt(col, this.row)) {
            computedAttr = computedAttr.blend(this.context.cursor.attribute);
        }
        if (this.offset <= col && this.context.mark && !endOfLine) {
            computedAttr = computedAttr.blend(this.context.mark.attrAt(col, this.row));
        }
        return computedAttr;
    }

    update() {
        let i = 0;
        for (i = 0; i < this.attrChars.length; ++i) {
            const attrChar = this.attrChars[i];
            const attr = this.computeAttr(attrChar.attribute, i, false);
            const c = attrChar.character;
            const span = this.spanIter.next();
            if (this.context.cursor.existsAt(i, this.row)) {
                span.id = MAIN_CURSOR_ID;
            } else {
                span.removeAttribute('id');
            }
            attr.updateElement(span);
            span.innerText = c;
        }

        if (this.context.cursor.existsAt(i, this.row)) {
            const attr = this.computeAttr(DEFAULT_ATTRIBUTE, i, true);
            const span = this.spanIter.next();
            span.id = MAIN_CURSOR_ID;
            attr.updateElement(span);
            span.innerText = ' ';
        }

        this.spanIter.purge();
    }
}

class SpanIterator {
    constructor(lineDiv) {
        this.lineDiv = lineDiv;
        this.node = lineDiv.firstChild;
    }

    next() {
        if (this.node instanceof HTMLBRElement) {
            const span = document.createElement('span');
            this.lineDiv.insertBefore(span, this.node);
            return span;
        } else {
            // lineDiv can have <span> or <br> only. So this.node isn't <br>, it must be <span>.
            const result = this.node;
            this.node = this.node.nextSibling;
            return result;
        }
    }

    purge() {
        const removeNodes = [];
        let node = this.node;
        while (!(node instanceof HTMLBRElement)) {
            removeNodes.push(node);
            node = node.nextSibling;
        }
        removeNodes.forEach((node) => {
            this.lineDiv.removeChild(node);
        });
    }
}

class TextLine {
    constructor(textEdit, row, attrChars = []) {
        this.textEdit = textEdit;
        this.row = row;
        this.attributedCharacters = attrChars;
    }

    get length() {
        return this.attributedCharacters.length;
    }

    splice(start, deleteCount, ...attrChars) {
        this.attributedCharacters.splice(start, deleteCount, ...attrChars);
        this.textEdit.requestUpdateRow(this.row);
    }

    slice(start = 0, end = this.attributedCharacters.length) {
        return this.attributedCharacters.slice(start, end);
    }

    splitAt(column) {
        const prevAttrChars = this.attributedCharacters.slice(0, column);
        const nextAttrChars = this.attributedCharacters.slice(column);
        return [prevAttrChars, nextAttrChars];
    }
}

const DEFAULT_ATTRIBUTE = new TextAttribute([], {});
const DEFAULT_MARK_ATTR = TextAttribute.fromStyleJson({
    'background-color': 'blue',
    'color': 'white'
});

const CURSOR_ATTRIBUTE_TABLE = {
    'block-blink': TextAttribute.fromCssClass('block-cursor-blink'),
    'block': TextAttribute.fromCssClass('block-cursor'),
    'underline-blink': TextAttribute.fromCssClass('underline-cursor-blink'),

    'underline': TextAttribute.fromCssClass('underline-cursor'),
    'vertical-blink': TextAttribute.fromCssClass('vertical-cursor-blink'),
    'vertical': TextAttribute.fromCssClass('vertical-cursor')
};

class TextEditRefreshController {
    constructor() {
        this.textEdits = new Set();

        window.addEventListener('focus', (e) => {
            this.textEdits.forEach((grutText) => {
                grutText.requestUpdateRow(grutText.cursor.row);
            });
        });

        window.addEventListener('blur', (e) => {
            this.textEdits.forEach((grutText) => {
                grutText.requestUpdateRow(grutText.cursor.row);
            });
        });

        registerAnimationFrameCallback((timestamp) => {
            this.refresh();
        });
    }

    register(textEdit) {
        this.textEdits.add(textEdit);
    }

    remove(textEdit) {
        this.textEdits.delete(textEdit);
    }

    refresh() {
        this.textEdits.forEach((textEdit) => {
            textEdit.refresh();
        });
    }
}

const textEditRefreshController = new TextEditRefreshController();

class TextMark {
    constructor(startCol, startRow, endCol, endRow) {
        this.startColumn = startCol;
        this.startRow = startRow;
        this.endColumn = endCol;
        this.endRow = endRow;
    }

    region() {
        if (this.startRow < this.endRow) {
            return [this.startColumn, this.startRow, this.endColumn, this.endRow];
        } else if (this.startRow === this.endRow) {
            if (this.startColumn <= this.endColumn) {
                return [this.startColumn, this.startRow, this.endColumn, this.endRow];
            } else {
                return [this.endColumn, this.endRow, this.startColumn, this.startRow];
            }
        } else {
            return [this.endColumn, this.endRow, this.startColumn, this.startRow];
        }
    }

    contains(col, row) {
        const [startCol, startRow, endCol, endRow] = this.region();

        if (startRow === endRow) {
            return startRow === row && startCol <= col && col < endCol;
        } else {
            return (row === startRow && startCol <= col) || (startRow < row && row < endRow) || (row === endRow && col < endCol);
        }
    }

    attrAt(col, row) {
        if (this.contains(col, row)) {
            return DEFAULT_MARK_ATTR;
        } else {
            return DEFAULT_ATTRIBUTE;
        }
    }
}

const TEXT_CONSOLE_STYLE = `
:host {
    display: block;
    outline: none;
    overflow-wrap: break-word;
    overflow-x: hidden;
    overflow-y: hidden;
    word-break: break-all;
    white-space: pre-wrap;
}

@keyframes block-cursor-blink-animation {
    0% {
        background: var(--grut-text-edit-background-color);
    }
    50% {
        background: var(--grut-text-edit-cursor-color);
    }
    100% {
        background: var(--grut-text-edit-background-color);
    }
}

.block-cursor-blink {
    animation-name: block-cursor-blink-animation;
    animation-duration: 1s;
    animation-timing-function: step-start;
    animation-delay: 0s;
    animation-iteration-count: infinite;
}

.block-cursor {
    background: var(--grut-text-edit-cursor-color)
}

@keyframes underline-cursor-blink-animation {
    0% {
        background: var(--grut-text-edit-background-color);
    }
    50% {
        background: linear-gradient(0deg, var(--grut-text-edit-cursor-color), var(--grut-text-edit-cursor-color) 10%, var(--grut-text-edit-background-color) 10%, var(--grut-text-edit-background-color));
    }
    100% {
        background: var(--grut-text-edit-background-color);
    }
}

.underline-cursor-blink {
    animation-name: underline-cursor-blink-animation;
    animation-duration: 1s;
    animation-timing-function: step-start;
    animation-delay: 0s;
    animation-iteration-count: infinite;
}

.underline-cursor {
    background: linear-gradient(0deg, var(--grut-text-edit-cursor-color), var(--grut-text-edit-cursor-color) 10%, var(--grut-text-edit-background-color) 10%, var(--grut-text-edit-background-color));
}

@keyframes vertical-cursor-blink-animation {
    0% {
        background: var(--grut-text-edit-background-color);
    }
    50% {
        background: linear-gradient(90deg, var(--grut-text-edit-cursor-color), var(--grut-text-edit-cursor-color) 4px, var(--grut-text-edit-background-color) 4px, var(--grut-text-edit-background-color));
    }
    100% {
        background: var(--grut-text-edit-background-color);
    }
}

.vertical-cursor-blink {
    animation-name: vertical-cursor-blink-animation;
    animation-duration: 1s;
    animation-timing-function: step-start;
    animation-delay: 0s;
    animation-iteration-count: infinite;
}

.vertical-cursor {
    background: linear-gradient(90deg, var(--grut-text-edit-cursor-color), var(--grut-text-edit-cursor-color) 4px, var(--grut-text-edit-background-color) 4px, var(--grut-text-edit-background-color));
}

.text-edit {
    min-width: 100%;
    min-height: 100%;
}

.input-area {
    opacity: 0;
    outline: none;
    caret-color: transparent;
    display: inline-block;
    white-space: pre;
    /* For IME */
    color: black;
    background-color: white;
}

@keyframes visual-bell-animation {
    100% {
        filter: invert(100);
        background: var(--grut-text-edit-background-color);
    }
}

.visual-bell {
    animation-name: visual-bell-animation;
    animation-duration: 0.1s;
    animation-timing-function: step-start;
    animation-delay: 0s;
    animation-iteration-count: 1;
}
`;

const BACKGROUND_COLOR_PROPERTY = '--grut-text-edit-background-color';
const COLOR_PROPERTY = '--grut-text-edit-color';
const CURSOR_COLOR_PROPERTY = '--grut-text-edit-cursor-color';

const PREFIX_KEY = 1;
const CALLBACK_WITH_CLIPBOARD = 2;

const CLIPBOARD_CUT = 1;
const CLIPBOARD_COPY = 2;
const CLIPBOARD_PASTE = 3;

class GrutText extends HTMLElement {
    constructor() {
        super();

        this.attrCharsList = [[]];
        this.startColumnList = [0];
        this.lineAttrList = [DEFAULT_ATTRIBUTE];
        this.updatedRowSet = new Set([0]);
        this.currentAttribute = DEFAULT_ATTRIBUTE;
        this.savedAttributes = [];
        this.mark = null;
        this.autoScrollHandler = undefined;
        this.isCompositionStarted = false;

        this.cursor = new TextCursor(this);

        this.inputArea = this.createInputArea();

        this.attachShadow({ mode: 'open', delegatesFocus: true });
        const style = document.createElement('style');
        style.textContent = TEXT_CONSOLE_STYLE;
        this.shadowRoot.appendChild(style);
        this.view = document.createElement('div');
        this.view.classList.add('text-edit');
        this.shadowRoot.appendChild(this.view);
        this.view.addEventListener('mousedown', (e) => {
            this.handleMouse(e, 'Down');
        });
        this.view.addEventListener('mouseup', (e) => {
            this.handleMouse(e, 'Up');
        });
        this.view.addEventListener('mousemove', (e) => {
            this.handleMouse(e, 'Move');
        });
        this.view.addEventListener('click', (e) => {
            this.handleMouse(e, 'Click');
        });
        this.view.addEventListener('dblclick', (e) => {
            this.handleMouse(e, 'DblClick');
        });
        this.view.addEventListener('mouseleave', (e) => {
            this.handleMouse(e, 'Leave');
        });
        this.view.addEventListener('cut', (e) => {
            this.copyMarkRegionToClipboard();
            this.clipboardCallback(CLIPBOARD_CUT);
            e.preventDefault();
        });
        this.view.addEventListener('copy', (e) => {
            this.copyMarkRegionToClipboard();
            this.clipboardCallback(CLIPBOARD_COPY);
            e.preventDefault();
        });
        this.view.addEventListener('paste', (e) => {
            this.clipboardCallback(CLIPBOARD_PASTE);
        });
        this.view.addEventListener('animationend', (e) => {
            if (e.animationName === 'visual-bell-animation') {
                this.view.classList.remove('visual-bell');
            }
        });

        this.inputEventCallback = undefined;
        this.keyEventCallback = undefined;
        this.keyEventAvailabilityTable = {};
        this.keyEventAvailabilityTableStack = [];
        this.keyEventPrefix = '';
        this.clipboardCallback = undefined;
        this.mouseEventCallback = undefined;
        this.pressingMouseButton = undefined;

        this.refresh();
    }

    connectedCallback() {
        textEditRefreshController.register(this);
        this.updateStyle();
    }

    disconnectedCallback() {
        textEditRefreshController.remove(this);
    }

    ///

    setCallbacks(inputEventCallback, keyEventCallback, clipboardCallback, mouseEventCallback) {
        this.inputEventCallback = inputEventCallback;
        this.keyEventCallback = keyEventCallback;
        this.clipboardCallback = clipboardCallback;
        this.mouseEventCallback = mouseEventCallback;
    }

    // availability := false | Integer
    setKeyEventAvailability(keyEvent, availability) {
        keyEvent.split(' ').slice(0, -1).reduce((prefix, evt) => {
            const newPrefix = (prefix === '') ? evt : `${prefix} ${evt}`;
            this.keyEventAvailabilityTable[newPrefix] = PREFIX_KEY;
            return newPrefix;
        }, '');
        this.keyEventAvailabilityTable[keyEvent] = availability;
    }

    clearKeyEventAvailability() {
        this.keyEventAvailabilityTable = {};
    }

    saveKeyEventAvailability() {
        this.keyEventAvailabilityTableStack.push(this.keyEventAvailabilityTable);
        this.keyEventAvailabilityTable = Object.assign({}, this.keyEventAvailabilityTable);
    }

    restoreKeyEventAvailability() {
        const tbl = this.keyEventAvailabilityTableStack.pop();
        if (tbl) {
            this.keyEventAvailabilityTable = tbl;
        }
    }

    ///

    convertColorNameToRGBA(colorName) {
        const canvas = document.createElement('canvas');
        canvas.width = 1;
        canvas.height = 1;
        const ctx = canvas.getContext('2d');
        ctx.fillStyle = colorName;
        ctx.fillRect(0, 0, 1, 1);
        return ctx.getImageData(0, 0, 1, 1).data;
    }

    updateStyle() {
        const viewStyle = window.getComputedStyle(this);
        this.view.style.setProperty(BACKGROUND_COLOR_PROPERTY, viewStyle['background-color']);
        const color = viewStyle.color;
        this.view.style.setProperty(COLOR_PROPERTY, color);
        const colorRGBA = this.convertColorNameToRGBA(color);
        const cursorColor = `rgba(${colorRGBA[0]},${colorRGBA[1]},${colorRGBA[2]},0.8)`;
        this.view.style.setProperty(CURSOR_COLOR_PROPERTY, cursorColor);
        const styleTabSize = viewStyle.tabSize;
        if (styleTabSize === '') {
            this.view.style.tabSize = '8';
            this.tabSize = 8;
        } else {
            this.tabSize = parseInt(styleTabSize);
        }

        this.updateCharacterSize();

        const widthColumn = parseInt(this.getAttribute('column'));
        if (widthColumn) {
            this.style.width = this.characterWidth * widthColumn;
            this.widthCharacter = widthColumn;
        } else {
            this.widthCharacter = undefined;
        }
        const heightRow = parseInt(this.getAttribute('row'));
        if (heightRow) {
            this.style.height = this.lineHeight * heightRow;
            this.heightCharacter = heightRow;
        } else {
            this.heightCharacter = undefined;
        }
    }

    updateCharacterSize() {
        const dummyDiv = document.createElement('div');
        dummyDiv.innerText = 'M';
        dummyDiv.style.display = 'inline-block';
        this.view.appendChild(dummyDiv);
        const charRect = dummyDiv.getBoundingClientRect();
        this.lineHeight = charRect.height;
        this.characterWidth = charRect.width;
        this.view.removeChild(dummyDiv);
    }

    static get observedAttributes() {
        return ['column', 'row'];
    }

    attributeChangedCallback(name, oldValue, newValue) {
        this.updateCharacterSize();
        switch (name) {
            case 'column':
                const widthColumn = parseInt(newValue);
                if (widthColumn) {
                    this.style.width = this.characterWidth * widthColumn;
                    this.widthCharacter = widthColumn;
                } else {
                    this.widthCharacter = undefined;
                }
                break;
            case 'row':
                const heightRow = parseInt(newValue);
                if (heightRow) {
                    this.style.height = this.lineHeight * heightRow;
                    this.heightCharacter = heightRow;
                } else {
                    this.heightCharacter = undefined;
                }
                break;
            default:
                break;
        }
    }

    get column() {
        const widthColumn = this.getAttribute('column');
        if (widthColumn) {
            return parseInt(widthColumn);
        } else {
            return undefined;
        }
    }

    set column(value) {
        this.setAttribute('column', value);
    }

    get row() {
        const heightRow = this.getAttribute('row');
        if (heightRow) {
            return parseInt(heightRow);
        } else {
            return undefined;
        }
    }

    set row(value) {
        this.setAttribute('row', value);
    }

    ///

    get bellStyle() {
        return this.getAttribute('bellstyle') || 'audible';
    }

    set bellStyle(style) {
        this.setAttribute('bellstyle', style);
    }

    get rows() {
        return this.attrCharsList.length;
    }


    get textContent() {
        return this.attrCharsList.map(this.attrChars2String).join('\n');
    }

    requestUpdateRow(...rows) {
        rows.forEach((row) => {
            this.updatedRowSet.add(row);
        });
    }

    requestAllUpdate() {
        for (let i = 0; i < this.rows; ++i) {
            this.updatedRowSet.add(i);
        }
    }

    line(row = this.cursor.row) {
        return new TextLine(this, row, this.attrCharsList[row]);
    }

    queryScreenSize() {
        const viewRect = this.view.getBoundingClientRect();
        return [
            Math.floor(this.widthCharacter || viewRect.width / this.characterWidth),
            Math.floor(this.heightCharacter || viewRect.height / this.lineHeight)
        ];
    }

    ///

    createInputArea() {
        const inputArea = document.createElement('span');
        inputArea.classList.add('input-area');
        inputArea.tabIndex = 0;
        inputArea.contentEditable = true;
        inputArea.innerText = '';

        inputArea.addEventListener('input', (e) => {
            this.handleInput(e);
        });
        inputArea.addEventListener('compositionstart', (e) => {
            this.handleCompositionStart(e);
        });
        inputArea.addEventListener('compositionend', (e) => {
            this.handleCompositionEnd(e);
        });
        inputArea.addEventListener('keydown', (e) => {
            this.handleKey(e);
        });

        return inputArea;
    }

    injectInputArea() {
        const cursor = this.shadowRoot.getElementById(MAIN_CURSOR_ID);
        if (cursor) {
            const parent = cursor.parentElement;
            parent.insertBefore(this.inputArea, cursor);
        }
    }

    removeInputArea() {
        const currentInputAreaLine = this.inputArea.parentElement;
        if (!currentInputAreaLine) {
            return;
        }
        currentInputAreaLine.removeChild(this.inputArea);
    }

    extractInputContent() {
        return this.inputArea.textContent.replace(/\n$/, '');
    }

    clearInputContent() {
        this.inputArea.innerText = '';
    }

    enableInputAreaIMEStyle() {
        this.inputArea.style.opacity = 1.0;
    }

    disableInputAreaIMEStyle() {
        this.inputArea.style.opacity = 0;
    }

    ///

    findLineDivAt(clientY) {
        const lineDivList = this.view.childNodes;
        for (let row = 0; row < lineDivList.length; ++row) {
            const lineDiv = lineDivList[row];
            const rect = lineDiv.getBoundingClientRect();
            if (rect.top <= clientY && clientY <= rect.bottom) {
                return [row, lineDiv];
            }
        }
        return [lineDivList.length - 1, lineDivList[lineDivList.length - 1]];
    }

    computeColumn(row, lineDiv, clientX, clientY) {
        let span = null;
        let offset = 0;
        const childNodes = lineDiv.childNodes;
        for (let i = 0; i < childNodes.length; ++i) {
            let node = childNodes[i];
            if (node instanceof HTMLSpanElement && !node.isContentEditable) {
                let rect = node.getBoundingClientRect();
                if (rect.left <= clientX && clientX <= rect.right && rect.top <= clientY && clientY <= rect.bottom) {
                    span = node;
                    break;
                }
                offset += Array.from(node.innerText).length;
            }
        }

        if (span) {
            const range = new Range();
            const text = span.firstChild;
            let charIndex = 0;
            let i = 0;
            while (i < text.length) {
                let code = text.data.charCodeAt(i);
                let end = i;
                if (0xd800 <= code && code <= 0xdfff) {
                    ++end;
                }
                range.setStart(text, i);
                range.setEnd(text, end + 1);
                let rect = range.getBoundingClientRect();
                if (rect.left <= clientX && clientX <= rect.right && rect.top <= clientY && clientY <= rect.bottom) {
                    return offset + charIndex;
                }
                i = end + 1;
                ++charIndex;
            }
        }

        const line = this.attrCharsList[row];
        if (line) {
            return line.length;
        } else {
            return undefined;
        }
    }

    computeTextPosition(clientX, clientY) {
        this.refresh();
        const [row, lineDiv] = this.findLineDivAt(clientY);
        if (lineDiv) {
            const col = this.computeColumn(row, lineDiv, clientX, clientY);
            if (col !== void 0) {
                return [col, row];
            }
        }
        return [undefined, undefined];
    }

    computeCharacterPositionAndSize(column, row) {
        this.refresh();
        const lineDivList = this.view.childNodes;
        const lineDiv = lineDivList[row];

        let span = null;
        let offset = 0;
        const childNodes = lineDiv.childNodes;
        for (let i = 0; i < childNodes.length; ++i) {
            let node = childNodes[i];
            if (node instanceof HTMLSpanElement && !node.isContentEditable) {
                const len = Array.from(node.innerText).length;
                if (offset <= column && column < offset + len) {
                    span = node;
                    break;
                }
                offset += len;
            }
        }

        if (span) {
            const range = new Range();
            const text = span.firstChild;
            let charIndex = 0;
            let i = 0;
            while (i < text.length) {
                const code = text.data.charCodeAt(i);
                let end = i;
                if (0xd800 <= code && code <= 0xdfff) {
                    ++end;
                }
                if (offset + charIndex === column) {
                    const code = text.data.charCodeAt(i);
                    range.setStart(text, i);
                    range.setEnd(text, end + 1);
                    const rect = range.getBoundingClientRect();
                    return [rect.x, rect.y, rect.width, rect.height];
                }
                i = end + 1;
                ++charIndex;
            }
        }
        return [false, false, false, false];
    }

    /// Editing operations

    eraseLineAfterCursor() {
        const line = this.line();
        const col = this.cursor.column;
        line.splice(col, line.length - col);
    }

    eraseLineBeforeCursor() {
        const line = this.line();
        const col = this.cursor.column;
        const len = Math.min(col + 1, line.length);
        const filler = new Array(len).fill(DEFAULT_ATTRIBUTE.withCharacter(' '));
        line.splice(0, len, ...filler);
    }

    eraseDisplayAfterCursor() {
        this.eraseLineAfterCursor();
        const numRows = this.rows;
        for (let i = numRows - 1; i > this.cursor.row; --i) {
            this.removeLine(i);
        }
    }

    eraseDisplayBeforeCursor() {
        this.eraseLineBeforeCursor();
        for (let i = 0; i < this.cursor.row; ++i) {
            const line = this.line(i);
            line.splice(0, line.length);
        }
    }

    eraseDisplay(n = 0) {
        switch (n) {
            // Clear from cursor to end of screen.
            case 0:
                this.eraseDisplayAfterCursor();
                break;
            // Clear from cursor to beginning of screen.
            case 1:
                this.eraseDisplayBeforeCursor();
                break;
            // Clear entire screen
            case 2:
                this.eraseDisplayBeforeCursor();
                this.eraseDisplayAfterCursor();
                break;
            default:
                break;
        }
    }

    eraseLine(n = 0) {
        switch (n) {
            // Clear from cursor to the end of the line.
            case 0:
                this.eraseLineAfterCursor();
                break;
            // Clear from cursor to the beginning of the line.
            case 1:
                this.eraseLineBeforeCursor();
                break;
            // Clear entire line.
            case 2:
                this.eraseLineBeforeCursor();
                this.eraseLineAfterCursor();
                break;
            default:
                break;
        }
    }

    visualBell() {
        this.view.classList.add('visual-bell');
    }

    appendEmptyLine(n = 1) {
        for (let i = 0; i < n; ++i) {
            this.attrCharsList.push([]);
            this.startColumnList.push(0);
            this.lineAttrList.push(DEFAULT_ATTRIBUTE);
            this.requestUpdateRow(this.rows - 1);
        }
    }

    setStartColumn(row = this.cursor.row, col = this.cursor.column) {
        this.startColumnList[row] = col;
    }

    insertLine(row) {
        this.attrCharsList.splice(row, 0, []);
        this.startColumnList.splice(row, 0, 0);
        this.lineAttrList.splice(row, 0, DEFAULT_ATTRIBUTE);
        for (let i = row; i < this.rows; ++i) {
            this.requestUpdateRow(i);
        }
    }

    removeLine(row = this.cursor.row) {
        this.attrCharsList.splice(row, 1);
        this.startColumnList.splice(row, 1);
        this.lineAttrList.splice(row, 1);
        if (this.attrCharsList.length === 0) {
            this.attrCharsList = [[]];
            this.startColumnList = [0];
            this.lineAttrList = [DEFAULT_ATTRIBUTE];
        }
        // Need to update the last deleted line, so the end must be this.rows + 1.
        for (let i = row; i < this.rows + 1; ++i) {
            this.requestUpdateRow(i);
        }
    }

    removeAllLines() {
        this.requestAllUpdate();
        this.attrCharsList = [[]];
        this.startColumnList = [0];
        this.lineAttrList = [DEFAULT_ATTRIBUTE];
        this.cursor.column = 0;
        this.cursor.row = 0;
    }

    attrChars2String(attrChars) {
        return attrChars.map((attrChar) => {
            return attrChar.character;
        }).join('');
    }

    extractMarkRegionText() {
        if (!this.mark) {
            return '';
        }

        let [startCol, startRow, endCol, endRow] = this.mark.region();
        startCol = Math.max(startCol, this.startColumnList[startRow]);
        endCol = this.startColumnList[endRow] <= endCol ? endCol : 0;
        if (startRow === endRow) {
            const line = this.line(startRow);
            const offset = this.startColumnList[startRow];
            return this.attrChars2String(line.slice(Math.max(offset, startCol), endCol));
        } else {
            let str = '';
            const firstLine = this.line(startRow);
            const firstOffset = this.startColumnList[startRow];
            str = this.attrChars2String(firstLine.slice(Math.max(firstOffset, startCol))) + '\n';
            for (let row = startRow + 1; row < endRow; ++row) {
                const offset = this.startColumnList[row];
                str += this.attrChars2String(this.attrCharsList[row].slice(offset)) + '\n';
            }
            const lastLine = this.line(endRow);
            const lastOffset = this.startColumnList[endRow];
            str += this.attrChars2String(lastLine.slice(lastOffset, endCol));
            return str;
        }
    }

    copyMarkRegionToClipboard() {
        const text = this.extractMarkRegionText();
        copyTextToClipboard(text);
    }

    processMarkRegion(remove) {
        if (!this.mark) {
            return '';
        }

        let str = '';
        const [startCol, startRow, endCol, endRow] = this.mark.region();
        if (startRow === endRow) {
            const line = this.line(startRow);
            str = this.attrChars2String(line.slice(startCol, endCol));
            if (remove) {
                line.splice(startCol, endCol - startCol);
            }
        } else {
            const lastLine = this.line(endRow);
            str = this.attrChars2String(lastLine.slice(0, endCol));
            if (remove) {
                lastLine.splice(0, endCol);
            }
            for (let row = endRow - 1; row >= startRow + 1; --row) {
                str = this.attrChars2String(this.attrCharsList[row]) + '\n' + str;
                if (remove) {
                    this.removeLine(row);
                }
            }
            const firstLine = this.line(startRow);
            str = this.attrChars2String(firstLine.slice(startCol)) + '\n' + str;
            if (remove) {
                firstLine.splice(startCol, firstLine.length - startCol);
                this.concatLine(startRow);
            }
        }
        if (remove) {
            this.cursor.column = startCol;
            this.cursor.row = startRow;
        }
        return str;
    }

    callWithClipboardText(proc) {
        if (navigator.clipboard) {
            navigator.clipboard.readText().then((text) => {
                proc(text);
            });
        } else if (window.clipboard) {
            proc(window.clipboard.readText());
        } else {
        }
    }

    setTextAttribute(cssClasses, styleJson) {
        this.currentAttribute = new TextAttribute(cssClasses || [], styleJson || {});
    }

    saveTextAttribute() {
        this.savedAttributes.push(this.currentAttribute);
    }

    restoreTextAttribute() {
        this.currentAttribute = this.savedAttributes.pop() || this.currentAttribute;
    }

    // TODO: Rename this method.
    updateTextAttribute(key, value) {
        const newStyle = Object.assign({}, this.currentAttribute.styleJson);
        if (value) {
            newStyle[key] = value;
        } else {
            delete newStyle[key];
        }
        this.currentAttribute = new TextAttribute(this.currentAttribute.cssClasses, newStyle);
    }

    setLineStyle(row, key, value) {
        const attr = this.lineAttrList[row];
        const newStyle = Object.assign({}, attr.styleJson);
        if (value) {
            newStyle[key] = value;
        } else {
            delete newStyle[key];
        }
        this.lineAttrList[row] = new TextAttribute(attr.cssClasses, newStyle);
        this.requestUpdateRow(row);
    }

    // TODO: Rename this method.
    updateCharacterAttributeInLine(fromColumn, toColumn, row, cssClasses, styleJson) {
        const attrChars = this.line(row);
        const attr = new TextAttribute(cssClasses || [], styleJson || {});
        for (let i = fromColumn; i <= toColumn; ++i) {
            if (attrChars.length <= i) {
                break;
            }
            attrChars[i].attribute = attr;
        }
        this.requestUpdateRow(row);
    }

    writeLine(str) {
        const attrChars = this.currentAttribute.withString(str);
        this.line().splice(this.cursor.column, attrChars.length, ...attrChars);
        this.cursor.column += attrChars.length;
    }

    updateLineString(row, startColumn, endColumn, str) {
        let deleteChars;
        if (endColumn >= 0) {
            deleteChars = endColumn - startColumn;
        } else {
            deleteChars = this.attrCharsList[row].length + endColumn + 1 - startColumn;
        }
        const attrChars = this.currentAttribute.withString(str);
        this.line(row).splice(startColumn, deleteChars, ...attrChars);
    }

    /// Cursor movement

    moveCursor(col, row) {
        if (col < 0 || row < 0) {
            throw new RangeError('Invalid column or row');
        }

        if (this.rows <= row) {
            const numAdditionalRows = row - this.rows + 1;
            for (let i = 0; i < numAdditionalRows; ++i) {
                this.appendEmptyLine();
            }
        }
        const line = this.line(row);
        if (line.length < col) {
            let filler = new Array(col - line.length).fill(DEFAULT_ATTRIBUTE.withCharacter(' '));
            line.splice(line.length, 0, ...filler);
        }
        this.cursor.column = col;
        this.cursor.row = row;
    }

    moveCursorUp(n) {
        this.moveCursor(this.cursor.col, Math.max(0, this.cursor.row - n));
    }

    moveCursorDown(n) {
        this.moveCursor(this.cursor.col, this.cursor.row + n);
    }

    moveCursorForward(n) {
        this.moveCursor(this.cursor.col + n, this.cursor.row);
    }

    moveCursorBack(n) {
        this.moveCursor(Math.max(0, this.cursor.col - n), this.cursor.row);
    }

    moveCursorNextLine(n) {
        this.moveCursor(0, this.cursor.row + n);
    }

    moveCursorPreviousLine(n) {
        this.moveCursor(0, Math.max(0, this.cursor.row - n));
    }

    moveCursorHorizontalAbsolute(n) {
        this.moveCursor(n, this.cursor.row);
    }


    /// Editing

    scrollUp(n = 1) {
        this.shadowRoot.host.scrollTop -= n * this.lineHeight;
    }

    scrollDown(n = 1) {
        this.shadowRoot.host.scrollTop += n * this.lineHeight;
    }

    scrollTo(row, alignToTop) {
        this.refresh();
        this.view.childNodes[row].scrollIntoView(alignToTop);
    }

    get pageSize() {
        return Math.max(1, Math.floor(this.shadowRoot.host.clientHeight / this.lineHeight) - 1);
    }

    setMark(startCol, startRow, endCol, endRow) {
        if (this.mark) {
            this.clearMark();
        }

        this.mark = new TextMark(startCol, startRow, endCol, endRow);
        const [, fromRow, , toRow] = this.mark.region();
        for (let row = fromRow; row <= toRow; ++row) {
            this.requestUpdateRow(row);
        }
    }

    clearMark() {
        if (!this.mark) {
            return;
        }

        const [, startRow, , endRow] = this.mark.region();
        for (let row = startRow; row <= endRow; ++row) {
            this.requestUpdateRow(row);
        }
        this.mark = null;
    }

    /// Refresh Console element.

    isRefreshNeeded() {
        return this.updatedRowSet.size > 0;
    }

    refresh() {
        if (this.updatedRowSet.size === 0) {
            return;
        }

        const isActive = document.activeElement === this;
        this.removeInputArea();

        const context = new UpdateContext(this.cursor, this.mark, isActive && document.hasFocus());
        const lineDivNodes = this.view.childNodes;
        this.updatedRowSet.forEach((row) => {
            const attrChars = this.attrCharsList[row];
            if (!attrChars) {
                Array.from(this.view.childNodes).slice(row).forEach((lineDiv) => {
                    this.view.removeChild(lineDiv);
                });
                return;
            }

            for (let i = 0; i < row - lineDivNodes.length + 1; ++i) {
                const newLineDiv = document.createElement('div');
                newLineDiv.appendChild(document.createElement('br'));
                this.view.appendChild(newLineDiv);
            }
            const lineDiv = lineDivNodes[row];
            this.lineAttrList[row].updateElement(lineDiv);
            const spanIter = new SpanIterator(lineDiv);
            const updater = new LineUpdater(context, this.startColumnList[row], row, attrChars, spanIter);
            updater.update();
        });
        this.updatedRowSet.clear();

        this.injectInputArea();

        if (isActive) {
            this.focus();
        }
    }

    // Keyboard event

    handleInputText(text) {
        if (this.inputEventCallback) {
            this.inputEventCallback(text);
        }
    }

    handleInput(event) {
        if (event.isComposing) {
            return;
        }

        const text = this.extractInputContent();
        this.handleInputText(text);
        this.clearInputContent();
    }

    handleCompositionStart(event) {
        this.isCompositionStarted = true;
        this.enableInputAreaIMEStyle();
    }

    handleCompositionEnd(event) {
        this.isCompositionStarted = false;
        // CompositionEnd event can be fired when the user is still typing.
        // In this case, modifying the input area will break the current IME state.
        // For the workaround, inserting text is moved to the next event loop
        // and not to insert the text if composition state is started.
        setTimeout(() => {
            if (this.isCompositionStarted) {
                return;
            }
            const text = this.extractInputContent();
            this.handleInputText(text);
            this.clearInputContent();
            this.disableInputAreaIMEStyle();
        }, 0);
    }

    keyboardEvent2String(keyboardEvent) {
        let str = '';
        if (keyboardEvent.altKey && keyboardEvent.key !== 'Alt') {
            str += 'A-';
        }
        if (keyboardEvent.metaKey && keyboardEvent.key !== 'Meta') {
            str += 'M-';
        }
        if (keyboardEvent.ctrlKey && keyboardEvent.key !== 'Control') {
            str += 'C-';
        }
        if (keyboardEvent.shiftKey && keyboardEvent.key !== 'Shift' && keyboardEvent.key.length > 1) {
            str += 'S-';
        }
        const key = keyboardEvent.key === ' ' ? 'Space' : keyboardEvent.key;
        return this.keyEventPrefix + str + key;
    }

    handleKeyEvent(event, eventName) {
        const availability = this.keyEventAvailabilityTable[eventName];
        let nextKeyEventPrefix = '';
        if (Number.isInteger(availability)) {
            event.preventDefault();

            if (availability & PREFIX_KEY) {
                nextKeyEventPrefix = `${this.keyEventPrefix}${eventName} `;
            }

            if (availability & CALLBACK_WITH_CLIPBOARD) {
                this.callWithClipboardText((text) => {
                    this.keyEventCallback(eventName, text);
                });
            } else {
                this.keyEventCallback(eventName, '');
            }
        } else if (this.keyEventPrefix !== '') {
            event.preventDefault();
            this.keyEventCallback(eventName, '');
        }
        this.keyEventPrefix = nextKeyEventPrefix;
    }

    handleKey(keyboardEvent) {
        if (keyboardEvent.isComposing) {
            return;
        }

        const eventName = this.keyboardEvent2String(keyboardEvent);
        this.handleKeyEvent(keyboardEvent, eventName);
    }

    /// Mouse event

    setAutoScroll(clientX, clientY) {
        if (this.autoScrollHandler) {
            clearTimeout(this.autoScrollHandler);
            this.autoScrollHandler = undefined;
        }

        if (this.pressingMouseButton === void 0) {
            return;
        }

        const SCROLL_LINE_PER_SEC = 10;
        const INTERVAL_MS = 100;
        const SCROLL_START_LINE = 1;
        const clientRect = this.shadowRoot.host.getBoundingClientRect();
        const yFromTop = clientY;
        const yFromBottom = (clientRect.bottom - clientRect.top) - (clientY + this.lineHeight);
        const deltaUnit = SCROLL_LINE_PER_SEC * this.lineHeight * INTERVAL_MS / 1000;
        const handler = () => {
            this.autoScrollHandler = undefined;
            const [col, row] = this.computeTextPosition(clientX, clientY);
            if (col !== void 0 && row !== void 0) {
                this.mouseEventCallback('Mouse-Drag-0', col, row);
            }
            this.setAutoScroll(clientX, clientY);
        };
        if (yFromTop < SCROLL_START_LINE * this.lineHeight) {
            this.shadowRoot.host.scrollTop -= deltaUnit * (1 - yFromTop / (SCROLL_START_LINE * this.lineHeight));
            this.autoScrollHandler = setTimeout(handler, INTERVAL_MS);
        } else if (yFromBottom < SCROLL_START_LINE * this.lineHeight) {
            this.shadowRoot.host.scrollTop += deltaUnit * (1 - yFromBottom / (SCROLL_START_LINE * this.lineHeight));
            this.autoScrollHandler = setTimeout(handler, INTERVAL_MS);
        }
    }

    mouseEvent2String(mouseEvent, eventType) {
        let keyPrefix = '';
        if (mouseEvent.altKey) {
            keyPrefix += 'A-';
        }
        if (mouseEvent.metaKey) {
            keyPrefix += 'M-';
        }
        if (mouseEvent.ctrlKey) {
            keyPrefix += 'C-';
        }
        if (mouseEvent.shiftKey) {
            keyPrefix += 'S-';
        }
        if (eventType === 'Move' && this.pressingMouseButton !== void 0) {
            return `Mouse-Drag-${this.pressingMouseButton}`;
        } else if (eventType === 'Move' || eventType === 'Leave') {
            return undefined;
        } else {
            return `${keyPrefix}Mouse-${eventType}-${mouseEvent.button}`;
        }
    }

    handleMouse(event, eventType) {
        switch (eventType) {
            case 'Down':
                this.pressingMouseButton = event.button;
                if (event.button === 0) {
                    this.focus();
                }
                break;
            case 'Move':
                this.setAutoScroll(event.clientX, event.clientY);
                break;
            case 'Up':
            case 'Leave':
                this.pressingMouseButton = undefined;
                break;
        }

        const eventName = this.mouseEvent2String(event, eventType);
        if (!eventName) {
            return;
        }

        const [col, row] = this.computeTextPosition(event.clientX, event.clientY);
        if (col === void 0 || row === void 0) {
            return;
        }

        if (this.mouseEventCallback) {
            this.mouseEventCallback(eventName, col, row);
            event.preventDefault();
        }
    }
}

customElements.define('grut-text', GrutText);
