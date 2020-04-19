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
            paragraph: {
                prefix: / */
            },
            blockquoteHeader: {
                prefix: /~ /,
                content: "inline"
            },
            blockquote: {
                prefix: /\| /,
                type: "spanning"
            },
            unorderedList: {
                prefix: /- /,
                type: "list"
            },
            orderedList: {
                prefix: /\d+\./,
                type: "list"
            },
            header: {
                prefix: /#+ /,
                content: "inline"
            },
            horizontalRule: {
                prefix: /==+/,
                content: "none"
            },
            codeBlock: {
                prefix: /::+/,
                type: "guarded",
                content: "none"
            },
            instruction: {
                prefix: /! /,
                content: "none"
            },
            comment: {
                prefix: /;+ /,
                content: "none"
            },
            embed: {
                prefix: /\[ /,
                content: "none"
            },
            footnote: {
                prefix: /[\d+]/,
                content: "inline"
            }
        },
        inlineDirectives: {
            bold: {
                prefix: /\*\*/
            },
            italic: {
                prefix: /\/\//
            },
            underline: {
                prefix: /__/
            },
            strikethrough: {
                prefix: /<-/,
                suffix: /->/
            },
            code: {
                prefix: /``/,
                content: "none"
            },
            supertext: {
                prefix: /\^\(/,
                suffix: /\)/
            },
            subtext: {
                prefix: /v\(/,
                suffix: /\)/
            },
            compound: {
                prefix: /"/,
                suffix: /"\(.*?\)/
            },
            footnoteReference: {
                prefix: /\[\d+\]/,
                suffix: ""
            },
            dash: {
                prefix: /---?/,
                suffix: ""
            },
            newline: {
                prefix: /-\/-/,
                suffix: ""
            }
        }
    };

    var copyDict = function(from, to){
        for(var prop in from)
            to[prop] = from[prop];
    };

    var normalizeLineDirective = function(directive){
        if(!directive.prefix)
            throw "Directive does not contain a prefix expression.";
        if(!(directive.prefix instanceof RegExp))
            directive.prefix = new RegExp(directive.prefix);
        if(!directive.type)
            directive.type = "singular";
        if(!directive.content)
            directive.content = "line";
    };

    var normalizeInlineDirective = function(directive){
        if(!directive.prefix)
            throw "Directive does not contain a prefix expression.";
        if(!(directive.prefix instanceof RegExp))
            directive.prefix = new RegExp(directive.prefix);
        if(directive.suffix === undefined)
            directive.suffix = directive.prefix;
        if(!(directive.suffix instanceof RegExp))
            directive.suffix = new RegExp(directive.suffix);
        if(!directive.content)
            directive.content = "inline";
    };

    CodeMirror.defineMode("markless", function(editorConf, config_) {
        var config = {lineDirectives: {}, inlineDirectives: {}};
        copyDict(defaultConfig.lineDirectives, config.lineDirectives);
        copyDict(defaultConfig.inlineDirectives, config.inlineDirectives);
        copyDict(config_.lineDirectives, config.lineDirectives);
        copyDict(config_.inlineDirectives, config.inlineDirectives);

        for(var prop in config.lineDirectives)
            normalizeLineDirective(config.lineDirectives[prop]);
        for(var prop in config.inlineDirectives)
            normalizeInlineDirective(config.inlineDirectives[prop]);

        return {
            startState: function() {
                return {
                    stack: [config.lineDirectives.paragraph],
                    top: 0
                };
            },
            
            blankLine: function(state) {
                if(state.stack[state.top].type != "guarded"){
                    state.stack = [config.lineDirectives.paragraph];
                    state.top = 0;
                }
            },

            token: function(stream, state) {
                var top = state.stack[state.top];
                switch(top.content){
                case "line":
                    // Try to parse more line directives. If not, fall to inline.
                    for(var prop in config.lineDirectives){
                        var directive = config.lineDirectives[prop];
                        if(stream.eat(directive.prefix) !== undefined){
                            state.top++;
                            state.stack[state.top] = directive;
                            return directive.style;
                        }
                    }
                case "inline":
                    // Try to end current directive first.
                    if(top.suffix && stream.eat(top.suffix) !== undefined){
                        state.top--;
                        return top.style;
                    }
                    // Then search for more.
                    for(var prop in config.inlineDirectives){
                        var directive = config.inlineDirectives[prop];
                        if(stream.eat(directive.prefix) !== undefined){
                            state.top++;
                            state.stack[state.top] = directive;
                            return directive.style;
                        }   
                    }
                    // No matches found, continue.
                    stream.next();
                    return top.style;
                    break;
                case "none":
                    if(top.suffix && stream.eat(top.suffix) !== undefined){
                        state.top--;
                        return top.style;
                    }
                    stream.next();
                    return top.style;
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
