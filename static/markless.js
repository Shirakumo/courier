(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
        mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
        define(["../../lib/codemirror"], mod);
    else // Plain browser env
        mod(CodeMirror);
})(function(CodeMirror) {
    "use strict";

    var defaultConfig = {
        lineDirectives: {
            blockquoteHeader: {
                prefix: "~ ",
                content: "inline",
                style: "def blockquote-header"
            },
            blockquote: {
                prefix: "\\| ",
                type: "spanning",
                style: "string blockquote"
            },
            unorderedList: {
                prefix: "- ",
                style: "unordered-list"
            },
            orderedList: {
                prefix: "\\d+\\.",
                style: "ordered-list"
            },
            header: {
                prefix: "#+ ",
                content: "inline",
                style: "header"
            },
            horizontalRule: {
                prefix: "==+",
                content: "none",
                style: "punctuation horizontal-rule"
            },
            codeBlock: {
                prefix: "::+",
                type: "guarded",
                content: "none",
                style: "builtin code-block"
            },
            instruction: {
                prefix: "! ",
                content: "none",
                style: "keyword instruction"
            },
            comment: {
                prefix: ";+ ",
                content: "none",
                style: "comment"
            },
            embed: {
                prefix: "\\[ ",
                content: "none",
                style: "meta embed"
            },
            footnote: {
                prefix: "\\[\\d+\\]",
                content: "inline",
                style: "footnote"
            }
        },
        inlineDirectives: {
            bold: {
                prefix: "\\*\\*",
                style: "variable bold"
            },
            italic: {
                prefix: "\\/\\/",
                style: "variable-2 italic"
            },
            underline: {
                prefix: "__",
                style: "variable-3 underline"
            },
            strikethrough: {
                prefix: "<-",
                suffix: "->",
                style: "strikethrough"
            },
            code: {
                prefix: "``",
                content: "none",
                style: "builtin code"
            },
            supertext: {
                prefix: "\\^\\(",
                suffix: "\\)",
                style: "supertext"
            },
            subtext: {
                prefix: "v\\(",
                suffix: "\\)",
                style: "subtext"
            },
            compound: {
                prefix: "\"",
                suffix: "\"\\(.*?\\)",
                style: "compound"
            },
            footnoteReference: {
                prefix: "\\[\\d+\\]",
                suffix: "",
                style: "number footnote-reference"
            },
            dash: {
                prefix: "---?",
                suffix: "",
                style: "punctuation dash"
            },
            newline: {
                prefix: "-/-",
                suffix: "",
                style: "punctuation newline"
            }
        }
    };

    var copyDict = function(from, to){
        for(var prop in from)
            to[prop] = from[prop];
    };

    var normalizeLineDirective = function(name, directive){
        if(!directive.prefix)
            throw "Directive does not contain a prefix expression.";
        if(!(directive.prefix instanceof RegExp))
            directive.prefix = new RegExp("^"+directive.prefix);
        if(!directive.type)
            directive.type = "singular";
        if(!directive.content)
            directive.content = "line";
        if(!directive.style)
            directive.style = "text";
        if(!directive.name)
            directive.name = name;
        directive.type = "line";
    };

    var normalizeInlineDirective = function(name, directive){
        if(!directive.prefix)
            throw "Directive does not contain a prefix expression.";
        if(!(directive.prefix instanceof RegExp))
            directive.prefix = new RegExp("^"+directive.prefix);
        if(directive.suffix === undefined)
            directive.suffix = directive.prefix;
        if(!(directive.suffix instanceof RegExp))
            directive.suffix = new RegExp("^"+directive.suffix);
        if(!directive.content)
            directive.content = "inline";
        if(!directive.style)
            directive.style = "text";
        if(!directive.name)
            directive.name = name;
        directive.type = "inline";
    };

    CodeMirror.defineMode("markless", function(editorConf, config_) {
        var config = {lineDirectives: {}, inlineDirectives: {}};
        copyDict(defaultConfig.lineDirectives, config.lineDirectives);
        copyDict(defaultConfig.inlineDirectives, config.inlineDirectives);
        copyDict(config_.lineDirectives, config.lineDirectives);
        copyDict(config_.inlineDirectives, config.inlineDirectives);

        for(var prop in config.lineDirectives){
            if(config.lineDirectives[prop])
                normalizeLineDirective(prop, config.lineDirectives[prop]);
            else delete config.lineDirectives[prop];
        }
        for(var prop in config.inlineDirectives){
            if(config.inlineDirectives[prop])
                normalizeInlineDirective(prop, config.inlineDirectives[prop]);
            else delete config.inlineDirectives[prop];
        }

        var computeStyle = function(state){
            var style = "";
            for(var i=0; i<=state.top; i++)
                style += " "+state.stack[i].style;
            return style;
        };

        var reset = function(state){
            state.stack = [{style: "text", content: "line"}];
            state.top = 0;
        };

        var push = function(state, directive){
            state.top++;
            state.stack[state.top] = directive;
            return computeStyle(state);
        };

        var pop = function(state){
            var style = computeStyle(state);
            state.top--;
            return style;
        };

        return {
            config: config,
            
            startState: function() {
                return {
                    stack: [{style: "text"}],
                    top: 0
                };
            },
            
            blankLine: function(state) {
                if(state.stack[state.top].type != "guarded"){
                    reset(state);
                }
            },

            token: function(stream, state) {
                if(stream.sol() && state.stack[state.top].type != "guarded")
                    reset(state);
                var top = state.stack[state.top];
                switch(top.content){
                case "line":
                    // Try to parse more line directives. If not, fall to inline.
                    for(var prop in config.lineDirectives){
                        var directive = config.lineDirectives[prop];
                        var match = stream.match(directive.prefix, true);
                        if(match)
                            return push(state, directive);
                    }
                case "inline":
                    // Try to end current directive first.
                    if(top.suffix && stream.match(top.suffix, true))
                        return pop(state);
                    // Then search for more.
                    for(var prop in config.inlineDirectives){
                        var directive = config.inlineDirectives[prop];
                        var match = stream.match(directive.prefix, true);
                        if(match){
                            // Poison base state to avoid further line matches.
                            state.stack[0].content = "inline";
                            return push(state, directive);
                        }
                    }
                    // No matches found, continue.
                    stream.next();
                    return computeStyle(state);
                    break;
                case "none":
                    if(stream.match(top.suffix || top.prefix, true))
                        return pop(state);
                    stream.next();
                    return computeStyle(state);
                    break;
                default:
                    throw "Bad parser state: no content marker on directive stack";
                    break;
                }
            },
            
            lineComment: "; "
        };
    });

    CodeMirror.defineMIME("text/markless", "markless");
    CodeMirror.defineMIME("text/x-markless", "markless");
});
