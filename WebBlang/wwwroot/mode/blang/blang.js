(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
      mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
      define(["../../lib/codemirror"], mod);
    else // Plain browser env
      mod(CodeMirror);
  })(function(CodeMirror) {
  "use strict";
  
  CodeMirror.defineSimpleMode("blang", {
      start: [
          {regex: /#.*$/, token: "comment"},
          {regex: /\d+\.?|[-+]?(?:\.\d+\.?|\d+\.?\d*)/, token: "number"},
          {regex: /"/, token: "string", next: "string"},
          {regex: /\(/, token: "bracket"},
          {regex: /\)/, token: "bracket"},
      ],
      string: [
        {regex: /.*?"/, token: "string", next: "start"},
        {regex: /.*/, token: "string"},
      ],
      meta: {
          lineComment: "#"
      }
  });
  
  CodeMirror.defineMIME("text/x-blang", "blang");
  
  });
  