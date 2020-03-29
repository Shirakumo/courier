class Courier{
    constructor(){
        var self = this;
        self.cache = {};
        if(console.log === undefined)
            self.log = ()=>{};
        else
            self.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Courier]");
                return console.log.apply(console, args);
            };

        self.log("Init");

        self.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!self.apiRoot){
            self.log("Failed to retrieve API root. WTF?");
        }

        self.registerAll(".type-select", self.registerTypeSelect);
        self.registerAll(".button.confirm", self.registerConfirm);
        self.registerAll(".editor", self.registerEditor);
        if(document.querySelector(".campaign.edit"))
            self.registerCampaignForm(document.querySelector(".campaign.edit"));
    }

    loadCSS(source){
        var self = this;
        return new Promise((ok)=>{
            var links = document.querySelectorAll("link[rel=stylesheet]");
            for(var i=0; i<links.length; i++){
                if(links[i].getAttribute("href") == source){
                    ok();
                    return;
                }
            }
            self.log("Loading", source);
            var el = self.constructElement("link",{
                attributes: {
                    type: "text/css",
                    rel: "stylesheet",
                    crossorigin: "anonymous",
                    href: source
                }
            });
            el.addEventListener("load", ok);
            document.querySelector("header").appendChild(el);
        });
    }

    loadJS(source){
        var self = this;
        return new Promise((ok)=>{
            var scripts = document.querySelectorAll("script");
            for(var i=0; i<scripts.length; i++){
                if(scripts[i].getAttribute("src") == source){
                    ok();
                    return;
                }
            }
            self.log("Loading", source);
            var el = self.constructElement("script",{
                attributes: {
                    type: "text/javascript",
                    crossorigin: "anonymous",
                    src: source
                }
            });
            el.addEventListener("load", ok);
            document.querySelector("body").appendChild(el);
        });
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(args instanceof HTMLElement){
                formData = new FormData(args);
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = (methodArgs.format=="json")? JSON.parse(request.responseText)
                    : request.responseText;
                if(request.status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            self.log("Sending request to",endpoint,args);
            request.open("POST", self.apiRoot+endpoint);
            request.send(formData);
        });
    }

    getListed(type, campaign){
        var self = this;
        if(self.cache[type] !== undefined){
            return new Promise((ok, fail)=>{
                ok(self.cache[type]);
            });
        }else{
            return self.apiCall(type+"/list", {campaign: campaign})
                .then((data)=>{self.cache[type] = data.data; return data.data;});
        }
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
            if(options.attributes[attr])
                el.setAttribute(attr, options.attributes[attr]);
        }
        (options.elements||[]).forEach(function(element){
            el.appendChild(self.constructElement(element.tag, element));
        });
        return el;
    }

    populateSelect(select, data, selectedValue){
        var self = this;
        select.innerHTML = "";
        for(var i=0; i<data.length; ++i){
            select.appendChild(self.constructElement("option",{
                attributes: {value: data[i]._id, selected: (data[i]._id == selectedValue)},
                text: data[i].title || data[i].name + " " + data[i].address
            }));
        }
    }

    registerAll(query, regger){
        var self = this;
        var elements = document.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    registerConfirm(element){
        element.addEventListener("click", (ev)=>{
            if(confirm("Are you sure?")){
                return true;
            }else{
                ev.preventDefault();
                return false;
            }
        });
    }

    registerTypeSelect(element){
        var self = this;

        var campaign = element.dataset.campaign;
        var type = element.querySelector("select");
        var id = element.querySelector("input");
        
        var select = self.constructElement("select", {
            attributes: {name: id.getAttribute("name")}
        });
        element.appendChild(select);
        element.removeChild(id);
        
        type.addEventListener("change", ()=>{
            var typeName =
                 (type.value == "0")? "mail"
                :(type.value == "1")? "link"
                :(type.value == "2")? "tag"
                :(type.value == "3")? "subscriber"
                :(type.value == "4")? "campaign"
                : undefined;
            self.getListed(typeName, campaign).then((data)=>self.populateSelect(select, data, id.value));
        });
        var evt = document.createEvent("HTMLEvents");
        evt.initEvent("change", false, true);
        type.dispatchEvent(evt);
    }

    instantiateTemplate(element){
        var copy = element.querySelector(".template").cloneNode(true);
        copy.classList.remove("template");
        copy.removeAttribute("data-name");
        [].forEach.call(copy.querySelectorAll("[data-name]"), (el)=>{
            el.setAttribute("name", el.dataset.name);
        });
        return copy;
    }

    registerCampaignForm(element){
        var self = this;
        var reg = (element)=>{
            element.querySelector(".remove-self").addEventListener("click",()=>{
                element.parentNode.removeChild(element);
            });
            return element;
        };
        [].forEach.call(element.querySelector(".attribute"), (el)=>{
            if(!el.classList.contains("template")) reg(el);
        });
        element.querySelector(".new-attribute").addEventListener("click",()=>{
            element.querySelector(".attributes").appendChild(reg(self.instantiateTemplate(element)));
        });
    }

    registerEditor(element){
        var self = this;
        var textarea = element.querySelector("textarea");
        var nav = element.querySelector("nav");
        var mode = (element.dataset.type == "html") ? "htmlmixed"
            :(element.dataset.type == "js") ? "javascript"
            :(element.dataset.type == "css") ? "css"
            :(element.dataset.type == "markless") ? "markless"
            : "";
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/codemirror.min.js")
            .then(()=>self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/mode/xml/xml.min.js"))
            .then(()=>self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/mode/htmlmixed/htmlmixed.min.js"))
            .then(()=>self.loadCSS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/codemirror.min.css"))
            .then(()=>self.loadCSS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/theme/mdn-like.min.css"))
            .then(()=>{
                let editor = CodeMirror.fromTextArea(textarea, {
                    mode: mode,
                    theme: "mdn-like",
                    lineNumbers: true,
                    lineWrapping: true,
                    viewportMargin: Infinity
                });
                editor.on("change", ()=>editor.save());});
        var preview = self.constructElement("iframe",{classes: ["preview", "hidden"]});
        element.appendChild(preview);
        preview.addEventListener("load", ()=>{
            preview.style.height = preview.contentWindow.document.body.scrollHeight + 'px';
        });
        if(nav.querySelector(".preview")){
            var previewEndpoint = element.dataset.previewEndpoint;
            nav.querySelector(".preview").addEventListener("click",()=>{
                if(preview.classList.contains("hidden")){
                    self.apiCall(previewEndpoint, element.closest("form"), {format:"html"})
                        .then((r)=>preview.src = "data:text/html;charset=utf-8,"+escape(r));
                    preview.classList.remove("hidden");
                    element.querySelector(".CodeMirror").classList.add("hidden");
                }else{
                    element.querySelector(".CodeMirror").classList.remove("hidden");
                    preview.classList.add("hidden");
                }
            });
        }
    }
}

var courier = new Courier();
