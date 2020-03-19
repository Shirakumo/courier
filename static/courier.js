class Courier{
    constructor(){
        var self = this;

        if(console.log === undefined)
            self.log = ()=>{};
        else
            self.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Courier]");
                return console.log.apply(console, args);
            };

        self.log("Init");
        
        [].forEach.call(document.querySelectorAll(".trigger"), (el)=>self.registerTrigger(el));
        document.querySelector(".add-trigger").addEventListener("click", function(){
            document.querySelector(".triggers").appendChild(self.registerTrigger(self.createTrigger()));
        });
    }

    constructElement(tag, options){
        var self = this;
        var el = document.createElement(options.tag || tag);
        (options.classes||[]).forEach(function(clas){
            if(clas) el.classList.add(clas);
        });
        if(options.text) el.innerText = options.text;
        if(options.html) el.innerHTML = options.html;
        for(var attr in (options.attributes||{})){
            el.setAttribute(attr, options.attributes[attr]);
        }
        (options.elements||[]).forEach(function(element){
            el.appendChild(self.constructElement(element.tag, element));
        });
        return el;
    };

    createTag(){
        return this.constructElement("li",{
            classes: ["tag"],
            elements: [
                {tag: "input", attributes:{type: "checkbox", name: "tag-inverted[]"}},
                {tag: "input", attributes:{type: "text", name: "tag-title[]", placeholder: "Tag"}},
                {tag: "a",
                 classes: ["button","remove-tag"],
                 elements: [{tag: "i", classes:["fas","fa-trash"]}]}
            ]
        });
    }

    createTrigger(){
        return this.constructElement("li",{
            classes: ["trigger"],
            elements: [
                {tag: "a",
                 classes: ["button", "remove-trigger"],
                 elements: [{tag: "i", classes: ["fas","fa-trash"]}]},
                {tag: "input", attributes:{type: "text", name: "to-mail[]", placeholder: "on mail..."}},
                {tag: "input", attributes:{type: "text", name: "to-link[]", placeholder: "on tag..."}},
                {tag: "input", attributes:{type: "number", name: "time-offset[]", step: "1", min: "0", placeholder:"with delay..."}},
                {tag: "span", text: "When user"},
                {tag: "input", attributes:{type: "hidden", name: "tag-count[]", value: "0"}},
                {tag: "ul", classes: ["tags"]},
                {tag: "a", 
                 classes: ["button", "add-tag"],
                 elements: [{tag: "i", classes: ["fas", "fa-plus-circle"]}]}
            ]
        });
    }
    
    registerTag(element){
        element.querySelector(".remove-tag").addEventListener("click", function(){
            var count = element.parentNode.parentNode.querySelector("[name=tag-count\\[\\]]");
            count.value = parseInt(count.value)-1;
            element.parentNode.removeChild(element);
        });
        return element;
    };

    
    registerTrigger(element){
        var self = this;
        [].forEach.call(element.querySelectorAll(".tag"), (el)=>self.registerTag(el));
        element.querySelector(".add-tag").addEventListener("click", function(){
            var count = element.querySelector("[name=tag-count\\[\\]]");
            count.value = parseInt(count.value)+1;
            element.querySelector(".tags").appendChild(self.registerTag(self.createTag()));
        });
        element.querySelector(".remove-trigger").addEventListener("click", function(){
            element.parentNode.removeChild(element);
        });
        return element;
    };
}

var courier = new Courier();
