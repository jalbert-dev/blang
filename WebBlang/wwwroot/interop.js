let IDE = null;

function updateTextArea(instance, _) {
    instance.save();
}

function getMultilineScriptText() {
    IDE.save();
    return document.getElementById("editor-textarea").value;
}

function setMultilineScriptText(value) {
    IDE.setValue(value);
}

function createIde() {
    IDE = CodeMirror.fromTextArea(
        document.getElementById("editor-textarea"),
        {
            lineNumbers: true,
            matchBrackets: true,
            autoCloseBrackets: true,
            styleActiveLine: true,
            mode: "blang",
        });

    IDE.getWrapperElement().classList.add("editor-wrapper");
    IDE.getWrapperElement().classList.add("ctrl-inside-bordered");
    IDE.getWrapperElement().classList.add("sensible-font-size");

    IDE.on("changes", updateTextArea);

    let loadingText = document.getElementById("editor-loading");
    loadingText.setAttribute("style", "display: none;");

    Split([".editor-container", ".repl-container"], { 
        minSize: 0,
        gutter: (_, direction) => {
            const gutter = document.createElement('div')
            gutter.id = "main-splitter"
            gutter.className = `gutter gutter-${direction}`
            return gutter
        },
        gutterSize: 8,
        minSize: 240,
        elementStyle: (_, elementSize, gutterSize, index) => {
            if (index === 0) {
                // "don't do anything besides return the style object in these 
                //  functions. Both of these functions should be pure"
                // yeah, sorry SplitJS guy, this splitter ain't gonna move itself
                // (not with a janky "position: absolute" based layout that is!)
                let splitter = document.getElementById("main-splitter");
                if (splitter) {
                    splitter.setAttribute("style", `left: calc(${elementSize}% - ${gutterSize}px);`);
                }

                return { 'right': `calc(${100 - elementSize}% + ${gutterSize}px)` }
            }
            else {
                return { 'left': `calc(${100 - elementSize}% + ${gutterSize}px)` }
            }
        },
        gutterStyle: (_, __, ___) => {
            return { 'width': '8px' }
        }
    });
}