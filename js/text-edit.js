
class TextEditRefreshController {
    constructor() {
        this.textEdits = new Set();

        window.requestAnimationFrame((timestamp) => {
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

        window.requestAnimationFrame((timestamp) => {
            this.refresh();
        });
    }
}

let textEditRefreshController = new TextEditRefreshController();

let DEFAULT_KEYMAP = {
    'Backspace': (textEdit) => {
        textEdit.backwardDeleteChar();
    },
    'Delete': (textEdit) => {
        textEdit.deleteChar();
    },
    'ArrowLeft': (textEdit) => {
        textEdit.previousChar();
    },
    'ArrowRight': (textEdit) => {
        textEdit.forwardChar();
    },
    'ArrowUp': (textEdit) => {
        textEdit.previousLine();
    },
    'ArrowDown': (textEdit) => {
        textEdit.nextLine();
    },
    'S-ArrowLeft': (textEdit) => {
        textEdit.previousChar(1, true);
    },
    'S-ArrowRight': (textEdit) => {
        textEdit.forwardChar(1, true);
    },
    'S-ArrowUp': (textEdit) => {
        textEdit.previousLine(1, true);
    },
    'S-ArrowDown': (textEdit) => {
        textEdit.nextLine(1, true);
    },
    'Home': (textEdit) => {
        textEdit.moveBeginningOfLine(false);
    },
    'End': (textEdit) => {
        textEdit.moveEndOfLine(false)
    },
    'S-Home': (textEdit) => {
        textEdit.moveBeginningOfLine(true);
    },
    'S-End': (textEdit) => {
        textEdit.moveEndOfLine(true)
    },
    'C-x': (textEdit) => {
        textEdit.processMarkRegion(true, true);
    },
    'C-c': (textEdit) => {
        textEdit.processMarkRegion(false, true);
    },
    'C-v': (textEdit) => {
        textEdit.paste();
    },
    'Tab': (textEdit) => {
        textEdit.insertText('\t');
    },
    'C-Space': (textEdit) => {
        textEdit.toggleMark();
    }
};

const DEFAULT_MOUSEMAP = {
    'Down-Button0': (textEdit, event) => {
        textEdit.startMouseSelection(event);
    },
    'Up-Button0': (textEdit, event) => {
        textEdit.endMouseSelection(event);
    },
    'Move': (textEdit, event) => {
        textEdit.updateMouseSelection(event);
    },
    'Leave': (textEdit, event) => {
        textEdit.endMouseSelection(event);
    }
};

class TextMark {
    constructor(cursor, fragile) {
        this.column = cursor.column;
        this.row = cursor.row;
        this.cursor = cursor;
        this.isFragile = fragile;
    }

    region() {
        let startCol;
        let startRow;
        let endCol;
        let endRow;

        if (this.row < this.cursor.row) {
            startRow = this.row;
            startCol = this.column;
            endRow = this.cursor.row;
            endCol = this.cursor.column;
        } else if (this.row === this.cursor.row) {
            if (this.column <= this.cursor.column) {
                startRow = this.row;
                startCol = this.column;
                endRow = this.cursor.row;
                endCol = this.cursor.column;
            } else {
                startRow = this.cursor.row;
                startCol = this.cursor.column;
                endRow = this.row;
                endCol = this.column;
            }
        } else if (this.cursor.row < this.row) {
            startRow = this.cursor.row;
            startCol = this.cursor.column;
            endRow = this.row;
            endCol = this.column;
        }

        return [startCol, startRow, endCol, endRow];
    }

    contains(col, row) {
        let [startCol, startRow, endCol, endRow] = this.region();

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
    background-color: black;
    color: white;
    display: block;
    font-family: Consolas, SFMono-Regular, 'Roboto Mono', 'Courier New', Courier, monospace;
    font-weight: normal;
    outline: none;
    overflow-wrap: break-word;
    overflow-x: auto;
    overflow-y: auto;
    word-break: break-all;
    white-space: pre-wrap;
}

@keyframes block-cursor-blink-animation {
    0% {
        background: var(--grv-text-edit-background-color);
    }
    50% {
        background: var(--grv-text-edit-cursor-color);
    }
    100% {
        background: var(--grv-text-edit-background-color);
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
    background: var(--grv-text-edit-cursor-color)
}

@keyframes underline-cursor-blink-animation {
    0% {
        background: var(--grv-text-edit-background-color);
    }
    50% {
        background: linear-gradient(0deg, var(--grv-text-edit-cursor-color), var(--grv-text-edit-cursor-color) 10%, var(--grv-text-edit-background-color) 10%, var(--grv-text-edit-background-color));
    }
    100% {
        background: var(--grv-text-edit-background-color);
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
    background: linear-gradient(0deg, var(--grv-text-edit-cursor-color), var(--grv-text-edit-cursor-color) 10%, var(--grv-text-edit-background-color) 10%, var(--grv-text-edit-background-color));
}

@keyframes vertical-cursor-blink-animation {
    0% {
        background: var(--grv-text-edit-background-color);
    }
    50% {
        background: linear-gradient(90deg, var(--grv-text-edit-cursor-color), var(--grv-text-edit-cursor-color) 4px, var(--grv-text-edit-background-color) 4px, var(--grv-text-edit-background-color));
    }
    100% {
        background: var(--grv-text-edit-background-color);
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
    background: linear-gradient(90deg, var(--grv-text-edit-cursor-color), var(--grv-text-edit-cursor-color) 4px, var(--grv-text-edit-background-color) 4px, var(--grv-text-edit-background-color));
}

.text-edit {
    width: 100%;
    height: 100%;
}

.input-area {
    opacity: 0;
    outline: none;
    caret-color: transparent;
    display: inline-block;
    /* For IME */
    color: black;
    background-color: white;
}
`

const BACKGROUND_COLOR_PROPERTY = '--grv-text-edit-background-color';
const COLOR_PROPERTY = '--grv-text-edit-color';
const CURSOR_COLOR_PROPERTY = '--grv-text-edit-cursor-color';

const SPAN_ROW = 'grv--row';
const SPAN_START_COLUMN = 'grv--start-column';

class GrvTextEdit extends HTMLElement {
    constructor() {
        super();

        this.attrCharsList = [[]];
        this.updatedRowSet = new Set([0]);
        this.currentAttribute = DEFAULT_ATTRIBUTE;
        this.keyMap = DEFAULT_KEYMAP;
        this.mark = null;
        this.mouseMap = DEFAULT_MOUSEMAP;
        this.isMouseSelecting = false;

        this.cursor = new TextCursor(this);

        this.inputArea = this.createInputArea();

        this.attachShadow({ mode: 'open', delegatesFocus: true });
        let style = document.createElement('style');
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
        this.view.addEventListener('mouseover', (e) => {
            this.handleMouse(e, 'Over');
        });
        this.view.addEventListener('mouseout', (e) => {
            this.handleMouse(e, 'Out');
        });
        this.view.addEventListener('mouseenter', (e) => {
            this.handleMouse(e, 'Enter');
        });
        this.view.addEventListener('mouseleave', (e) => {
            this.handleMouse(e, 'Leave');
        });

        this.refresh();
    }

    connectedCallback() {
        textEditRefreshController.register(this);
        this.initStyle();
    }

    disconnectedCallback() {
        textEditRefreshController.remove(this);
    }

    convertColorNameToRGBA(colorName) {
        let canvas = document.createElement('canvas');
        canvas.width = 1;
        canvas.height = 1;
        let ctx = canvas.getContext('2d');
        ctx.fillStyle = colorName;
        ctx.fillRect(0, 0, 1, 1);
        return ctx.getImageData(0, 0, 1, 1).data;
    }

    initStyle() {
        let viewStyle = window.getComputedStyle(this);
        this.view.style.setProperty(BACKGROUND_COLOR_PROPERTY, viewStyle['background-color']);
        let color = viewStyle['color'];
        this.view.style.setProperty(COLOR_PROPERTY, color);
        let colorRGBA = this.convertColorNameToRGBA(color);
        let cursorColor = `rgba(${colorRGBA[0]},${colorRGBA[1]},${colorRGBA[2]},0.8)`;
        this.view.style.setProperty(CURSOR_COLOR_PROPERTY, cursorColor);
        let styleTabSize = viewStyle['tabSize'];
        if (styleTabSize === '') {
            this.view.style['tabSize'] = '8';
            this.tabSize = 8;
        } else {
            this.tabSize = parseInt(styleTabSize);
        }
        this.isSoftTab = false;
        this.foregroundColor = viewStyle['color'];
        this.backgroundColor = viewStyle['backgroundColor'];
    }

    ///

    get rows() {
        return this.attrCharsList.length;
    }

    /**
     * @param {TextAttribute} attr
     */
    set attribute(attr) {
        this.currentAttribute = attr;
    }

    /**
     * @param {any} style
     */
    set style(style) {
        let styleJson;
        if (style instanceof CSSStyleDeclaration) {
            styleJson = {};
            for (let i = 0; i < style.length; ++i) {
                let key = style.item(i);
                styleJson[key] = style[key];
            }
        } else {
            styleJson = style;
        }
        this.currentAttribute = new TextAttribute(this.currentAttribute.cssClasses, styleJson);
    }

    /**
     * @param {[string]} cssClasses
     */
    set cssClasses(cssClasses) {
        this.currentAttribute = new TextAttribute(cssClasses, this.currentAttribute.cssStyleJson);
    }

    requestUpdateRow(...rows) {
        rows.forEach((row) => {
            this.updatedRowSet.add(row);
        })
    }

    line(row = this.cursor.row) {
        return new TextLine(this, row, this.attrCharsList[row]);
    }

    ///

    createInputArea() {
        let inputArea = document.createElement('span');
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
        let cursor = this.shadowRoot.getElementById(MAIN_CURSOR_ID);
        if (cursor) {
            let parent = cursor.parentElement;
            parent.insertBefore(this.inputArea, cursor);
        }
    }

    removeInputArea() {
        let currentInputAreaLine = this.inputArea.parentElement;
        if (!currentInputAreaLine) {
            return;
        }
        currentInputAreaLine.removeChild(this.inputArea);
    }

    extractInputContent() {
        if (!this.inputArea.firstChild) {
            return '';
        }

        return this.inputArea.firstChild.textContent;
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
        let lineDivList = this.view.childNodes;
        for (let row = 0; row < lineDivList.length; ++row) {
            let lineDiv = lineDivList[row];
            let rect = lineDiv.getBoundingClientRect();
            if (rect.top <= clientY && clientY <= rect.bottom) {
                return [row, lineDiv];
            }
        }
        return [undefined, undefined];
    }

    computeColumn(row, lineDiv, clientX, clientY) {
        let span = null;
        let offset = 0;
        let childNodes = lineDiv.childNodes;
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
            let range = new Range();
            let text = span.firstChild;
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

        let line = this.attrCharsList[row];
        if (line) {
            return line.length;
        } else {
            return undefined;
        }
    }

    computeTextPosition(clientX, clientY) {
        this.refresh();
        let [row, lineDiv] = this.findLineDivAt(clientY);
        if (lineDiv) {
            let col = this.computeColumn(row, lineDiv, clientX, clientY);
            if (col !== void 0) {
                return [col, row];
            }
        }
        return [undefined, undefined];
    }

    /// Editing operations

    moveCursorFreely(col, row) {
        if (col < 0 || row < 0) {
            throw new RangeError('Invalid column or row');
        }

        if (this.rows <= row) {
            let numAdditionalRows = row - this.rows + 1;
            for (let i = 0; i < numAdditionalRows; ++i) {
                this.appendEmptyLine();
            }
        }
        let line = this.line(row);
        if (line.length < col) {
            let filler = new Array(col - line.length).fill(DEFAULT_ATTRIBUTE.withCharacter(' '));
            line.splice(line.length, 0, ...filler);
        }
        this.cursor.column = col;
        this.cursor.row = row;
    }

    splitLine() {
        let column = this.cursor.column;
        let row = this.cursor.row;
        let line = this.line();
        this.attrCharsList.splice(row, 1, line.slice(0, column), line.slice(column));
        for (let i = row; i < this.rows; ++i) {
            this.requestUpdateRow(i);
        }
    }

    concatLine(row = this.cursor.row) {
        let line = this.line(row);
        line.splice(line.length, 0, ...this.attrCharsList[row + 1]);
        this.removeLine(row + 1);
    }

    appendEmptyLine() {
        this.attrCharsList.push([]);
        this.requestUpdateRow(this.rows - 1);
    }

    removeLine(row = this.cursor.row) {
        this.attrCharsList.splice(row, 1);
        // Need to update the last deleted line, so the end must be this.rows + 1.
        for (let i = row; i < this.rows + 1; ++i) {
            this.requestUpdateRow(i);
        }
    }

    attrChars2String(attrChars) {
        return attrChars.map((attrChar) => {
            return attrChar.character;
        }).join('');
    }

    computeTabSpaceWidthAt(column) {
        return this.tabSize - (column % this.tabSize);
    }

    copyToClipboard(str) {
        let activeElement = document.activeElement;
        let textArea = document.createElement('textarea');
        textArea.value = str;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        textArea.parentElement.removeChild(textArea);
        activeElement.focus();
    }

    processMarkRegion(remove, copyToClipboard) {
        if (!this.mark) {
            return '';
        }

        let str = '';
        let [startCol, startRow, endCol, endRow] = this.mark.region();
        if (startRow === endRow) {
            let line = this.line(startRow);
            str = this.attrChars2String(line.slice(startCol, endCol));
            if (remove) {
                line.splice(startCol, endCol - startCol);
            }
        } else {
            let lastLine = this.line(endRow);
            str = this.attrChars2String(lastLine.slice(0, endCol));
            lastLine.splice(0, endCol);
            for (let row = endRow - 1; row >= startRow + 1; --row) {
                str = this.attrChars2String(this.attrCharsList[row]) + str;
                if (remove) {
                    this.removeLine(row);
                }
            }
            let firstLine = this.line(startRow);
            str = firstLine.slice(startCol) + str;
            if (remove) {
                firstLine.splice(startCol, firstLine.length - startCol);
                this.concatLine(startRow);
            }
        }
        this.clearMark();
        if (remove) {
            this.cursor.column = startCol;
            this.cursor.row = startRow;
        }
        if (copyToClipboard) {
            navigator.clipboard.writeText(str);
        }
        return str;
    }

    paste() {
        navigator.clipboard.readText().then((text) => {
            this.insertText(text);
        });
    }

    insertCharacter(c) {
        if (this.mark) {
            this.processMarkRegion(true, false);
        }

        let attr = this.currentAttribute;
        switch (c) {
            case '\n':
                this.insertNewline();
                break;
            case '\t':
                if (this.isSoftTab) {
                    let spaceLen = this.computeTabSpaceWidthAt(this.cursor.column);
                    this.line().splice(this.cursor.column, 0, ...attr.withCharacters(new Array(spaceLen).fill(' ')));
                    this.cursor.column += spaceLen;
                } else {
                    this.line().splice(this.cursor.column, 0, attr.withCharacter(c));
                    ++this.cursor.column;
                }
                break;
            case '\b':
                this.backwardDeleteChar();
                break;
            case '\r':
                this.moveBeginningOfLine();
                break;
            case '\u007f':
                this.deleteChar();
                break;
            default:
                this.line().splice(this.cursor.column, 0, attr.withCharacter(c));
                ++this.cursor.column;
                break;
        }
    }

    insertText(text) {
        let interpreter = new ANSIEscapeSequenceInterpreter(this);
        interpreter.processString(text);
    }

    insertNewline() {
        if (this.mark) {
            this.processMarkRegion(true, false);
        }

        this.splitLine();
        this.cursor.column = 0;
        this.cursor.row += 1;
    }

    backwardDeleteChar(n = 1) {
        if (this.mark) {
            this.processMarkRegion(true, false);
            return;
        }

        for (let i = 0; i < n; ++i) {
            if (this.cursor.atBeginningOfConsole()) {
                return i;
            } else if (this.cursor.atBeginningOfLine()) {
                this.cursor.row -= 1;
                this.cursor.column = this.line().length;
                this.concatLine();
            } else {
                this.cursor.column -= 1;
                this.line().splice(this.cursor.column, 1);
            }
        }
        return n;
    }

    deleteChar(n = 1) {
        if (this.mark) {
            this.processMarkRegion(true, false);
            return;
        }

        for (let i = 0; i < n; ++i) {
            if (this.cursor.atEndOfConsole()) {
                return i;
            } else if (this.cursor.atEndOfLine()) {
                this.concatLine();
            } else {
                this.line().splice(this.cursor.column, 1);
            }
        }
        return n;
    }

    updateMarkBeforeCursorMovementIfNeeded(shiftMark) {
        if (shiftMark && !this.mark) {
            this.setMark(true);
        } else if (!shiftMark && this.mark && this.mark.isFragile) {
            this.clearMark();
        }
    }

    forwardChar(n = 1, shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        for (let i = 0; i < n; ++i) {
            if (this.cursor.atEndOfConsole()) {
                return i;
            } else if (this.cursor.atEndOfLine()) {
                this.cursor.column = 0;
                this.cursor.row += 1;
            } else {
                this.cursor.column += 1;
            }
        }
        return n;
    }

    previousChar(n = 1, shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        for (let i = 0; i < n; ++i) {
            if (this.cursor.atBeginningOfConsole()) {
                return i;
            } else if (this.cursor.atBeginningOfLine()) {
                this.cursor.row -= 1;
                this.cursor.column = this.line().length;
            } else {
                this.cursor.column -= 1;
            }
        }
        return n;
    }

    computeTabAwareLengthFromColumn(column, row) {
        let attrChars = this.attrCharsList[row];
        let taLen = 0;
        let i;
        for (i = 0; i < attrChars.length; ++i) {
            if (i === column) {
                return taLen;
            }
            if (attrChars[i].character === '\t') {
                taLen += this.tabSize - (taLen % this.tabSize);
            } else {
                ++taLen;
            }
        }
        return taLen + (column - i);
    }

    computeColumnFromTabAwareLength(taLen, row) {
        let attrChars = this.attrCharsList[row];
        let l = 0;
        let i;
        for (i = 0; i < attrChars.length; ++i) {
            if (taLen <= l) {
                return i;
            }
            if (attrChars[i].character === '\t') {
                l += this.tabSize - (l % this.tabSize);
            } else {
                ++l;
            }
        }
        return i + (taLen - l);
    }

    nextLine(n = 1, shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        let curRow = this.cursor.row;
        let taLen = this.computeTabAwareLengthFromColumn(this.cursor.rawColumn, curRow);
        let newRow = this.cursor.row + n;
        if (this.rows <= newRow) {
            newRow = this.rows - 1;
        }
        this.cursor.row = newRow;
        this.cursor.column = this.computeColumnFromTabAwareLength(taLen, newRow);
        return newRow - curRow;
    }

    previousLine(n = 1, shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        let curRow = this.cursor.row;
        let taLen = this.computeTabAwareLengthFromColumn(this.cursor.rawColumn, curRow);
        let newRow = this.cursor.row - n;
        if (newRow < 0) {
            newRow = 0;
        }
        this.cursor.row = newRow;
        this.cursor.column = this.computeColumnFromTabAwareLength(taLen, newRow);
        return curRow - newRow;
    }

    moveBeginningOfLine(shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        this.cursor.column = 0;
    }

    moveEndOfLine(shiftMark = false) {
        this.updateMarkBeforeCursorMovementIfNeeded(shiftMark);

        this.cursor.column = this.line().length;
    }

    setMark(transient) {
        this.mark = new TextMark(this.cursor, transient);
    }

    clearMark() {
        if (!this.mark) {
            return;
        }

        let [, startRow, , endRow] = this.mark.region();
        for (let row = startRow; row <= endRow; ++row) {
            this.requestUpdateRow(row);
        }
        this.mark = null;
    }

    toggleMark() {
        if (this.mark) {
            this.clearMark();
        } else {
            this.setMark(false);
        }
    }

    startMouseSelection(event) {
        let [col, row] = this.computeTextPosition(event.clientX, event.clientY);
        if (col === void 0 || row === void 0) {
            return;
        }
        this.cursor.column = col;
        this.cursor.row = row;
        this.clearMark();
        this.isMouseSelecting = true;
    }

    updateMouseSelection(event) {
        if (!this.isMouseSelecting) {
            return;
        }

        let [col, row] = this.computeTextPosition(event.clientX, event.clientY);
        if (col === void 0 || row === void 0) {
            return;
        }
        this.cursor.column = col;
        this.cursor.row = row;
        if (!this.mark) {
            this.setMark(true);
        }
    }

    endMouseSelection(event, copyToClipboard) {
        this.isMouseSelecting = false;
        if (copyToClipboard) {
            this.processMarkRegion(false, true);
        }
    }

    /// Refresh Console element.

    isRefreshNeeded() {
        return this.updatedRowSet.size > 0;
    }

    refresh() {
        if (this.updatedRowSet.size === 0) {
            return;
        }

        let hasFocus = document.activeElement === this;
        this.removeInputArea();

        let context = new UpdateContext(this.cursor, this.mark);
        let lineDivNodes = this.view.childNodes;
        this.updatedRowSet.forEach((row) => {
            let attrChars = this.attrCharsList[row];
            if (!attrChars) {
                Array.from(this.view.childNodes).slice(row).forEach((lineDiv) => {
                    this.view.removeChild(lineDiv);
                })
                return;
            }

            for (let i = 0; i < row - lineDivNodes.length + 1; ++i) {
                let newLineDiv = document.createElement('div');
                newLineDiv.appendChild(document.createElement('br'));
                this.view.appendChild(newLineDiv);
            }
            let spanIter = new SpanIterator(lineDivNodes[row]);
            let updater = new LineUpdater(context, row, attrChars, spanIter);
            updater.update();
        });
        this.updatedRowSet.clear();

        this.injectInputArea();

        if (hasFocus) {
            this.focus();
        }
    }

    // Keyboard event

    handleInput(event) {
        if (!event.isComposing) {
            this.insertText(this.extractInputContent());
            this.clearInputContent();
        }
    }

    handleCompositionStart(event) {
        this.enableInputAreaIMEStyle();
    }

    handleCompositionEnd(event) {
        this.insertText(this.extractInputContent());
        this.clearInputContent();
        this.disableInputAreaIMEStyle();
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
        if (keyboardEvent.shiftKey && keyboardEvent.key !== 'Shift') {
            str += 'S-';
        }
        let key = keyboardEvent.key === ' ' ? 'Space' : keyboardEvent.key;
        return str + key;
    }

    handleKey(keyboardEvent) {
        if (keyboardEvent.isComposing) {
            return;
        }

        if (keyboardEvent.key === 'Unidentified') {
            return;
        }

        let eventName = this.keyboardEvent2String(keyboardEvent);
        let handler = this.keyMap[eventName];
        if (handler) {
            keyboardEvent.preventDefault();
            handler(this, keyboardEvent);
        }
    }

    /// Mouse event

    mouseEvent2String(mouseEvent, eventType) {
        if (eventType === 'Enter' || eventType === 'Leave' || eventType === 'Move' || eventType === 'Out' || eventType === 'Over') {
            return eventType;
        }

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
        return `${keyPrefix}${eventType}-Button${mouseEvent.button}`;
    }

    handleMouse(event, eventType) {
        let eventName = this.mouseEvent2String(event, eventType);
        if (eventName === 'Down-Button0') {
            //            this.inputArea.focus();
            this.focus();
        }

        let handler = this.mouseMap[this.mouseEvent2String(event, eventType)];
        if (handler) {
            event.preventDefault();
            handler(this, event);
        }
    }
}

customElements.define('grv-text-edit', GrvTextEdit);

class ANSIEscapeSequenceInterpreter {
    constructor(textEdit) {
        this.textEdit = textEdit;
        this.cursorSaveStack = [];
    }

    processString(string) {
        let chars = Array.from(string);
        while (chars.length > 0) {
            let c = chars.shift();
            if (c === '\u001b') {
                this.processEscapeSequence(chars);
            } else {
                this.textEdit.insertCharacter(c);
            }
        }
    }

    processEscapeSequence(chars) {
        let c = chars.shift();
        switch (c) {
            case '[':
                this.processCSISequence(chars);
                break;
            default:
                return;
        }
    }

    processCSISequence(chars) {
        let stack = [];
        this.parseCSIParams(chars, stack);
        let c = chars.shift();
        switch (c) {
            // Cursor Up
            case 'A':
                this.textEdit.moveCursorFreely(
                    this.cursor.column,
                    Math.max(0, this.cursor.row - (stack[0] || 1)));
                break;
            // Cursor Down
            case 'B':
                this.textEdit.moveCursorFreely(
                    this.cursor.column,
                    this.cursor.row + (stack[0] || 1));
                break;
            // Cursor Forward
            case 'C':
                this.textEdit.moveCursorFreely(
                    this.cursor.column + 1,
                    this.cursor.row);
                break;
            // Cursor Back
            case 'D':
                this.textEdit.moveCursorFreely(
                    Math.max(0, this.cursor.column - 1),
                    this.cursor.row);
                break;
            // Cursor Next Line
            case 'E':
                this.textEdit.moveCursorFreely(
                    0,
                    this.cursor.row + (stack[0] || 1));
                break;
            // Cursor Previous Line
            case 'F':
                this.textEdit.moveCursorFreely(
                    0,
                    Math.max(0, this.cursor.row - (stack[0] || 1)));
                break;
            // Cursor Horizontal Absolute
            case 'G':
                this.textEdit.moveCursorFreely(
                    (stack[0] || this.textEdit.cursor.column + 1) - 1,
                    this.cursor.row);
                break;
            // Cursor Position
            case 'H':
            case 'f':
                this.textEdit.moveCursorFreely((stack[1] || 1) - 1, (stack[0] || 1) - 1);
                break;
            // Erase in Display
            case 'J':
                this.eraseDisplay(stack[0] || 0);
                break;
            // Erase in Line
            case 'K':
                this.eraseLine(stack[0] || 0);
                break;
            // Select Graphic Rendition
            case 'm':
                this.textEdit.attribute = this.parseAttribute(stack);
                break;
            // Save Current Cursor Position
            case 's':
                this.cursorSaveStack.push([this.textEdit.cursor.column, this.textEdit.cursor.row]);
                break;
            // Restore Saved Cursor Position
            case 'u':
                if (this.cursorSaveStack.length > 0) {
                    let [col, row] = this.cursorSaveStack.pop();
                    this.textEdit.cursor.column = col;
                    this.textEdit.cursor.row = row;
                }
                break;
            default:
                return;
        }
    }

    parseCSIParams(chars, stack) {
        while (chars.length > 0 && ('0' <= chars[0] && chars[0] <= '9')) {
            this.parseNumber(chars, stack);
            if (chars[0] === ';') {
                chars.shift();
                continue;
            } else {
                return;
            }
        }
    }

    parseNumber(chars, stack) {
        if (chars.length === 0 || !('0' <= chars[0] && chars[0] <= '9')) {
            return;
        }
        let n = 0;
        while (chars.length > 0) {
            let c = chars[0];
            if ('0' <= c && c <= '9') {
                n = 10 * n + (c.charCodeAt(0) - 0x30);
                chars.shift();
            } else {
                stack.push(n);
                return;
            }
        }
    }

    clearLineAfterCursor() {
        let line = this.textEdit.line();
        let col = this.textEdit.cursor.column;
        line.splice(col, line.length - col);
    }

    clearLineBeforeCursor() {
        let line = this.textEdit.line();
        let col = this.textEdit.cursor.column;
        let filler = new Array(col + 1).fill(DEFAULT_ATTRIBUTE.withCharacter(' '));
        line.splice(0, Math.min(col + 1, line.length), ...filler);
    }

    clearScreenAfterCursor() {
        this.clearLineAfterCursor();
        let numRows = this.textEdit.rows;
        for (let i = numRows - 1; i > this.cursor.row; --i) {
            this.removeLine(i);
        }
    }

    clearScreenBeforeCursor() {
        this.clearLineBeforeCursor();
        for (let i = 0; i < this.cursor.row; ++i) {
            let line = this.textEdit.line(i);
            line.splice(0, line.length);
        }
    }

    eraseDisplay(mode = 0) {
        switch (mode) {
            // Clear from cursor to end of screen.
            case 0:
                this.clearScreenAfterCursor();
                break;
            // Clear from cursor to beginning of screen.
            case 1:
                this.clearScreenBeforeCursor();
                break;
            // Clear entire screen
            case 2:
            case 3:
                this.clearScreenBeforeCursor();
                this.clearScreenAfterCursor();
                break;
            default:
                break;
        }
    }

    eraseLine(mode = 0) {
        switch (mode) {
            // Clear from cursor to the end of the line.
            case 0:
                this.clearLineAfterCursor();
                break;
            // Clear from cursor to the beginning of the line.
            case 1:
                this.clearLineBeforeCursor();
                break;
            // Clear entire line.
            case 2:
                this.clearLineBeforeCursor();
                this.clearLineAfterCursor();
                break;
            default:
                break;
        }
    }

    parseAttribute(stack) {
        let foregroundColor = '';
        let backgroundColor = '';
        let intensity = false;
        let italic = false;
        let underline = false;
        let reverse = false;

        while (stack.length > 0) {
            let code = stack.shift();
            switch (code) {
                // Reset/Normal
                case 0:
                    foregroundColor = '';
                    backgroundColor = '';
                    intensity = false;
                    italic = false;
                    underline = false;
                    reverse = false;
                    break;
                // Increased intensity
                case 1:
                    intensity = true;
                    break;
                // Decreased intensity
                case 2:
                    intensity = false;
                    break;
                // Italic
                case 3:
                    italic = true;
                    break;
                // Underline
                case 4:
                    underline = true;
                    break;
                // Reverse video
                case 7:
                    reverse = true;
                    break;
                // Normal color or intensity
                case 22:
                    foregroundColor = '';
                    backgroundColor = '';
                    intensity = false;
                    break;
                // Not italic
                case 23:
                    italic = false;
                    break;
                // Underline off
                case 24:
                    underline = false;
                    break;
                // Reverse off
                case 27:
                    reverse = false;
                    break;
                // Set foreground color
                case 30:
                case 31:
                case 32:
                case 33:
                case 34:
                case 35:
                case 36:
                case 37:
                    foregroundColor = this.parseBasicColor(code - 30 + (intensity ? 8 : 0));
                    break;
                // Set foreground color (8 or 24 bit)
                case 38:
                    foregroundColor = this.parseColor(stack);
                    break;
                // Default foreground color
                case 39:
                    foregroundColor = '';
                    break;
                // Set background color
                case 40:
                case 41:
                case 42:
                case 43:
                case 44:
                case 45:
                case 46:
                case 47:
                    backgroundColor = this.parseBasicColor(code - 40 + (intensity ? 8 : 0));
                    break;
                // Set background color (8 or 24 bit)
                case 48:
                    backgroundColor = this.parseColor(stack);
                    break;
                // Default background color
                case 49:
                    backgroundColor = '';
                    break;
                default:
                    break;
            }
        }

        let style = {};
        if (reverse) {
            style['color'] = backgroundColor || this.textEdit.backgroundColor;
            style['background-color'] = foregroundColor || this.textEdit.foregroundColor;
        } else {
            if (foregroundColor) {
                style['color'] = this.color2rgb(foregroundColor);
            }
            if (backgroundColor) {
                style['background-color'] = this.color2rgb(backgroundColor);
            }
        }
        if (italic) {
            style['font-style'] = 'italic';
        }
        if (underline) {
            style['text-decoration'] = 'underline';
        }

        return TextAttribute.fromStyleJson(style);
    }

    // TODO: Make the color customizable
    color2rgb(name) {
        let table = {
            'black': 'rgb(0,0,0)',
            'red': 'rgb(170,0,0)',
            'green': 'rgb(0,170,0)',
            'yellow': 'rgb(170,85,0)',
            'blue': 'rgb(0,0,170)',
            'magenta': 'rgb(170,0,170)',
            'cyan': 'rgb(0,170,170)',
            'white': 'rgb(170,170,170)',
            'gray': 'rgb(85,85,85)',
            'brightred': 'rgb(255,85,85)',
            'brightgreen': 'rgb(85,255,85)',
            'brightyellow': 'rgb(255,255,85)',
            'brightblue': 'rgb(85,85,255)',
            'brightmagenta': 'rgb(255,85,255)',
            'brightcyan': 'rgb(85,255,255)',
            'brightwhite': 'rgb(255,255,255)'
        };
        return table[name] || name;
    }

    parseBasicColor(n) {
        let colorTable = [
            'black',
            'red',
            'green',
            'yellow',
            'blue',
            'magenta',
            'cyan',
            'white',
            'gray',
            'brightred',
            'brightgreen',
            'brightyellow',
            'brightblue',
            'brightmagenta',
            'brightcyan',
            'brightwhite'
        ];
        return colorTable[n] || '';
    }

    parseColor(stack) {
        let mode = stack.shift();
        switch (mode) {
            // 8 bit
            case 5: {
                let n = stack.shift();
                if (0 <= n && n <= 15) {
                    return this.parseBasicColor(n);
                } else if (16 <= n && n <= 231) {
                    let v = n - 16;
                    let b = v % 6;
                    let g = ((v - b) / 6) % 6;
                    let r = ((v - b - 6 * g) / 36);
                    return `rgb(${Math.ceil((255 / 6) * (r + 1))},${Math.ceil((255 / 6) * (g + 1))},${Math.ceil((255 / 6) * (b + 1))})`;
                } else if (232 <= n && n <= 255) {
                    let v = Math.ceil((255 / 24) * (n - 231));
                    return `rgb(${v},${v},${v})`;
                } else {
                    return '';
                }
            }
            // 24 bit
            case 2: {
                let r = Math.min(stack.shift() || 0, 255);
                let g = Math.min(stack.shift() || 0, 255);
                let b = Math.min(stack.shift() || 0, 255);
                return `rgb(${r},${g},${b})`;
            }
            default:
                return '';
        }
    }
}

class UpdateContext {
    constructor(cursor, mark) {
        this.cursor = cursor;
        this.mark = mark;
    }
}

class LineUpdater {
    constructor(context, row, attrChars, spanIter) {
        this.context = context;
        this.row = row;
        this.attrChars = attrChars;
        this.spanIter = spanIter;
    }

    computeAttr(attr, col, endOfLine) {
        let computedAttr = attr;
        if (this.context.cursor.existsAt(col, this.row)) {
            computedAttr = computedAttr.blend(this.context.cursor.attribute);
        }
        if (this.context.mark && !endOfLine) {
            computedAttr = computedAttr.blend(this.context.mark.attrAt(col, this.row));
        }
        return computedAttr;
    }

    update() {
        let i = 0;
        for (i = 0; i < this.attrChars.length; ++i) {
            let attrChar = this.attrChars[i];
            let attr = this.computeAttr(attrChar.attribute, i, false);
            let c = attrChar.character;
            let span = this.spanIter.next();
            if (this.context.cursor.existsAt(i, this.row)) {
                span.id = MAIN_CURSOR_ID;
            } else {
                span.removeAttribute('id'); 
            }
            attr.updateSpan(span);
            span.innerText = c;
        }

        if (this.context.cursor.existsAt(i, this.row)) {
            let attr = this.computeAttr(DEFAULT_ATTRIBUTE, i, true);
            let span = this.spanIter.next();
            span.id = MAIN_CURSOR_ID;
            attr.updateSpan(span);
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
            let span = document.createElement('span');
            this.lineDiv.insertBefore(span, this.node);
            return span;
        } else {
            // lineDiv can have <span> or <br> only. So this.node isn't <br>, it must be <span>.
            let result = this.node;
            this.node = this.node.nextSibling;
            return result;
        }
    }

    purge() {
        let removeNodes = [];
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

const MAIN_CURSOR_ID = 'grv-main-cursor-id';

class TextCursor {
    constructor(textEdit) {
        this.textEdit = textEdit;
        this._column = 0;
        this._row = 0;
        this._mode = 'vertical-blink';
        this.visible = true;
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
        let prevRow = this.row;
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
    }

    get attribute() {
        if (this.visible) {
            return CURSOR_ATTRIBUTE_TABLE[this._mode];
        } else {
            return DEFAULT_ATTRIBUTE;
        }
    }

    atBeginningOfLine() {
        return this.column === 0;
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
    }

    hide() {
        this.visible = false;
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
        let prevAttrChars = this.attributedCharacters.slice(0, column);
        let nextAttrChars = this.attributedCharacters.slice(column);
        return [prevAttrChars, nextAttrChars];
    }
}

class AttributedCharacter {
    constructor(attr, char) {
        this.attribute = attr;
        this.character = char;
    }
}

class TextAttribute {
    constructor(cssClasses, cssStyleJson) {
        this.cssClasses = cssClasses;
        this.cssStyleJson = cssStyleJson;
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
        return new TextAttribute(this.cssClasses.concat(cssClasses), this.cssStyleJson);
    }

    blend(attr) {
        let newCssClasses = this.cssClasses.concat(attr.cssClasses);
        let newStyleJson = Object.assign({}, this.cssStyleJson);
        Object.keys(attr.cssStyleJson).forEach((key) => {
            newStyleJson[key] = attr.cssStyleJson[key];
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

        let thisCssStyleKeys = Object.keys(this.cssStyleJson);
        let thatCssStyleKeys = Object.keys(obj.cssStyleJson);
        if (thisCssStyleKeys.length !== thatCssStyleKeys.length) {
            return false;
        }
        return thisCssStyleKeys.every((key) => {
            return this.cssStyleJson[key] === obj.cssStyleJson[key];
        });
    }

    updateSpan(span) {
        span.removeAttribute('class');
        this.cssClasses.forEach((cssClass) => {
            if (cssClass.length > 0) {
                span.classList.add(cssClass);
            }
        })
        span.removeAttribute('style');
        Object.keys(this.cssStyleJson).forEach((key) => {
            let value = this.cssStyleJson[key];
            span.style[key] = value;
            switch (key) {
                case 'background-color':
                    span.style.setProperty(BACKGROUND_COLOR_PROPERTY, value);
                    break;
                case 'color':
                    span.style.setProperty(COLOR_PROPERTY, value);
                    break;
                default:
                    break;
            }
        });
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

///

function initText() {
    let textEdit = document.getElementById('console');
    textEdit.cursor.mode = 'vertical-blink';
    textEdit.insertText('Hello, world!0123\n456\u001b[31mfoobarbaz\u001b[1;32m\u001b[2;3Habc');
    textEdit.focus();
}

window.addEventListener('load', initText);
