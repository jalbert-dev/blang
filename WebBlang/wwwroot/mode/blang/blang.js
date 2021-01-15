(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
      mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
      define(["../../lib/codemirror"], mod);
    else // Plain browser env
      mod(CodeMirror);
  })(function(CodeMirror) {
  "use strict";

  function createMainState(parenCssName, parentCssName, cycleNext) {
    return [
      {regex: /(?:bind-value|bind-function|eval|'|\[\]|\+|-|\/|\*|%|=|mod|if)/, token: "atom"},
      {regex: /#.*$/, token: "comment"},
      {regex: /\d+\.?|[-+]?(?:\.\d+\.?|\d+\.?\d*)/, token: "number"},
      {regex: /"/, token: "string", push: "string"},
      {regex: /\(/, token: parenCssName, push: cycleNext},
      {regex: /\)/, token: parentCssName, pop: true},
    ]
  }
  
  CodeMirror.defineSimpleMode("blang", {
      start: createMainState("parens-1", "parens-3", "start2"),
      start2: createMainState("parens-2", "parens-1", "start3"),
      start3: createMainState("parens-3", "parens-2", "start"),
      string: [
        {regex: /.*?"/, token: "string", pop: true},
        {regex: /.*/, token: "string"},
      ],
      meta: {
          lineComment: "#"
      }
  });
  
  CodeMirror.defineMIME("text/x-blang", "blang");
  
  });
  