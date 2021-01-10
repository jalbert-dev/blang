let IDE = null;

function updateTextArea(instance, _) {
    instance.save();
}

function createIde() {
    IDE = CodeMirror.fromTextArea(
        document.getElementById("editor-textarea"),
        {
            lineNumbers: true
        });

    IDE.getWrapperElement().classList.add("editor-wrapper");
    IDE.getWrapperElement().classList.add("ctrl-inside-bordered");
    IDE.getWrapperElement().classList.add("sensible-font-size");

    IDE.on("changes", updateTextArea);

    let loadingText = document.getElementById("editor-loading");
    loadingText.setAttribute("style", "display: none;");
}