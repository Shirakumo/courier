class Courier{
    constructor(){
        var self = this;
        self.cache = {};
        self.loading = {};
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

        var url = [location.protocol, '//', location.host, location.pathname].join('');

        if(sessionStorage.getItem("formStorageUrl") == url){
            self.reloadForms(JSON.parse(sessionStorage.getItem("formStorage")));
        }
        
        window.addEventListener("beforeunload", ()=>{
            sessionStorage.setItem("formStorage", JSON.stringify(self.serializeForms()));
            sessionStorage.setItem("formStorageUrl", url);
        });

        self.registerElements();
    }

    registerElements(element){
        element = element || document;
        var self = this;
        self.registerAll(element, ".type-select", self.registerTypeSelect);
        self.registerAll(element, ".button.confirm", self.registerConfirm);
        self.registerAll(element, ".editor", self.registerEditor);
        self.registerAll(element, ".chart", self.registerChart);
        self.registerAll(element, "form", self.registerForm);
        self.registerAll(element, ".dynamic-list", self.registerDynamicList);
        self.registerAll(element, ".tag-list", self.registerTags);
        self.registerAll(element, ".access-level", self.registerAccess);
    }

    loadCSS(source){
        var self = this;
        if(!self.loading[source])
            self.loading[source] = new Promise((ok)=>{
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
        return self.loading[source];
    }

    loadJS(source){
        var self = this;
        if(!self.loading[source])
            self.loading[source] = new Promise((ok)=>{
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
        return self.loading[source];
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = self.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            self.log("Sending request to",endpoint);
            request.open("POST", endpoint);
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

    showPopup(content){
        var self = this;
        var popup = self.constructElement("div",{
            classes: ["popup"],
        });
        popup.hide = ()=>{
            popup.parentNode.removeChild(popup);
        };
        popup.appendChild(content);
        popup.addEventListener("click", (ev)=>{
            if(ev.target == popup)popup.hide();
        });
        document.querySelector("body").appendChild(popup);
        return popup;
    }

    showError(content){
        var box = document.querySelector(".box.error");
        var updated = box.cloneNode(true);
        updated.innerText = content;
        box.parentNode.replaceChild(updated, box);
    }

    populateSelect(select, data, selectedValue){
        var self = this;
        select.innerHTML = "";
        for(var i=0; i<data.length; ++i){
            select.appendChild(self.constructElement("option",{
                attributes: {value: data[i]._id, selected: (data[i]._id == selectedValue)},
                text: data[i].title || data[i].url || data[i].name + " " + data[i].address
            }));
        }
    }

    registerAll(element, query, regger){
        var self = this;
        var elements = element.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    registerForm(element){
        var self = this;
        var save = element.querySelector("input[type=submit]");
        if(!save) return;
        if(element.classList.contains("search")) return;
        save.addEventListener("click", (ev)=>{
            ev.preventDefault();
            if(element.checkValidity()){
                self.apiCall(save.getAttribute("formaction") || element.getAttribute("action"), element)
                    .then((r)=>{window.location.replace(r.target);},
                          (r)=>{self.showError(r.message ||
                                               new DOMParser().parseFromString(r, "text/html").querySelector("title").innerText);});
            }else{
                element.reportValidity();
            }
            return false;
        });
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
                 (type.value == "0"||type.value == "10")? "mail"
                :(type.value == "1")? "link"
                :(type.value == "2")? "tag"
                :(type.value == "3")? "subscriber"
                :(type.value == "4")? "campaign"
                :(type.value == "5")? "file"
                :(type.value == "6")? "sequence"
                :(type.value == "7")? "trigger"
                :(type.value == "8")? "host"
                : null;
            if(typeName)
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

    registerDynamicList(element){
        var self = this;
        var reg = (element)=>{
            self.registerElements(element);
            var remove = element.querySelector(".remove-self");
            if(remove)
                remove.addEventListener("click",()=>{
                    element.parentNode.removeChild(element);
                });
            return element;
        };
        [].forEach.call(element.querySelectorAll("li"), (el)=>{
            if(!el.classList.contains("template")) reg(el);
        });
        element.querySelector("a.new").addEventListener("click",()=>{
            element.querySelector("ul").appendChild(reg(self.instantiateTemplate(element)));
        });
    }

    registerTags(element){
        var self = this;
        var reg = (element, id, title)=>{
            element.querySelector(".remove-self").addEventListener("click",()=>{
                element.parentNode.removeChild(element);
            });
            if(id) element.querySelector("input").value = id;
            if(title) element.querySelector(".title").innerText = title;
            return element;
        };
        [].forEach.call(element.querySelectorAll("li"), (el)=>{
            if(!el.classList.contains("template")) reg(el);
        });
        element.querySelector("a.new").addEventListener("click",()=>{
            var option = element.querySelector("select option:checked");
            if(!element.querySelector("input[value=\""+option.value+"\"]"))
                element.querySelector("ul").appendChild(reg(self.instantiateTemplate(element), option.value, option.innerText));
        });
    }

    registerEditor(element){
        var self = this;
        var textarea = element.querySelector("textarea");
        var nav = element.querySelector("nav");
        var type = element.dataset.type.toLowerCase();
        var mode = 
             (type == "html") ? "htmlmixed"
            :(type == "ctml") ? "htmlmixed"
            :(type == "js") ? "javascript"
            :(type == "css") ? "css"
            :(type == "markless") ? "markless"
            : "";
        var editor = null;
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/codemirror.min.js")
            .then(()=>self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/mode/xml/xml.min.js"))
            .then(()=>self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/mode/htmlmixed/htmlmixed.min.js"))
            .then(()=>self.loadJS("/static/courier/markless.js"))
            .then(()=>self.loadCSS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/codemirror.min.css"))
            .then(()=>self.loadCSS("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.2/theme/mdn-like.min.css"))
            .then(()=>self.loadCSS("/static/courier/markless.css"))
            .then(()=>{
                editor = CodeMirror.fromTextArea(textarea, {
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
                        .then((r)=>{
                            preview.srcdoc = r;
                            preview.classList.remove("hidden");
                            element.querySelector(".CodeMirror").classList.add("hidden");
                        },(r)=>self.showError(r.message));
                }else{
                    element.querySelector(".CodeMirror").classList.remove("hidden");
                    preview.classList.add("hidden");
                }
            });
        }
        if(nav.querySelector(".upload")){
            nav.querySelector(".upload").addEventListener("click",()=>{
                var el = self.showPopup(self.constructElement("form",{
                    classes: ["image","upload"],
                    elements: [
                        {tag: "input", attributes: {type:"file", accept:"image/png,image/jpeg,image/gif,.png,.jpg,.jpeg,.gif"}},
                        {tag: "canvas"},
                        {tag: "i", classes: ["fas", "fa-expand-alt"]},
                        {tag: "input", attributes: {type:"number", class:"width", min:"1"}},
                        {tag: "input", attributes: {type:"number", class:"height", min:"1"}},
                        {tag: "button", attributes: {type:"button", class:"upload"}, text: "Upload"}
                    ]
                }));
                self.registerFileUpload(el, (formdata)=>{
                    formdata.append("campaign", element.closest("form").querySelector("input[name=campaign]").value);
                    self.apiCall("file/new", formdata).then((r)=>{
                        if(mode == "markless")
                            editor.replaceRange("\n[ image "+r.data.url+" ]\n", editor.getDoc().getCursor());
                        else if(mode == "htmlmixed")
                            editor.replaceRange("\n<img src=\""+r.data.url+"\" alt=\""+r.data.filename+"\">\n", editor.getDoc().getCursor());
                        else
                            editor.replaceRange("\n"+r.data.url+"\n", editor.getDoc().getCursor());
                        el.hide();
                    });
                });
            });
        }
    }

    registerChart(element){
        var self = this;
        var ctx = element.querySelector("canvas").getContext("2d");
        var chart = null;
        var refresh = ()=>
            self.apiCall(element.getAttribute("action"),element)
            .then((r)=>{
                chart.data.labels = r.data.labels;
                chart.data.datasets[0].data = r.data.points;
                chart.update();
            });
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js")
            .then(()=>self.loadJS("https://cdn.jsdelivr.net/npm/chartjs-plugin-colorschemes"))
            .then(()=>{
                chart = new Chart(ctx, {
                    type: element.dataset.type,
                    data: {
                        datasets: [{
                            data: [],
                            pointRadius: 10
                        }],
                        labels: []
                    },
                    options: {
                        layout: {padding: 5},
                        legend: {display: false},
                        scales: {
                            yAxes: [{
                                ticks: {suggestedMin: 0}
                            }]
                        },
                        plugins: {
                            colorschemes: {scheme: 'tableau.Classic20'}
                        }
                    }
                });
                if(element.dataset.type == "bar")
                    chart.options.scales.yAxes[0].ticks.suggestedMax = 1.0;
                refresh();
            });
        [].forEach.call(element.querySelectorAll("select"), (el)=>{
            el.addEventListener("change", refresh);
        });
    }

    registerFileUpload(element, onSubmit){
        var self = this;
        var input = element.querySelector("input[type=file]");
        var canvas = element.querySelector("canvas");
        var width = element.querySelector(".width");
        var height = element.querySelector(".height");
        var submit = element.querySelector("button.upload");
        var ctx = canvas.getContext("2d");
        var img = null;
        
        input.addEventListener("change", ()=>{
            var file = input.files[0];
            var fr = new FileReader();
            fr.onload = ()=>{
                img = new Image();
                img.onload = ()=>{
                    canvas.width = img.width;
                    canvas.height = img.height;
                    width.value = img.width;
                    height.value = img.height;
                    ctx.drawImage(img,0,0);
                };
                img.src = fr.result;
            };
            fr.readAsDataURL(file);
        });
        var changeDimensions = ()=>{
            canvas.width = parseInt(width.value);
            canvas.height = parseInt(height.value);
            ctx.drawImage(img,0,0,canvas.width,canvas.height);
        };
        width.addEventListener("change", ()=>{
            var aspect = img.height / img.width;
            height.value = Math.floor(aspect * parseInt(width.value));
            changeDimensions();
        });
        height.addEventListener("change", ()=>{
            var aspect = img.width / img.height;
            width.value = Math.floor(aspect * parseInt(height.value));
            changeDimensions();
        });
        submit.addEventListener("click", (ev)=>{
            ev.preventDefault();
            // Create temp canvas at full res
            var tmpCanvas = document.createElement('canvas');
            tmpCanvas.width = parseInt(width.value);
            tmpCanvas.height = parseInt(height.value);
            
            var ctx = tmpCanvas.getContext('2d');
            ctx.drawImage(img,0,0,canvas.width,canvas.height);
            tmpCanvas.toBlob((blob)=>{
                var formdata = new FormData();
                formdata.append("file", blob, input.files[0].name);
                onSubmit(formdata);
            });
            return false;
        });
    }

    registerAccess(element){
        var display = element.parentNode.querySelector(".access-level-desc");
        var updateAccess = ()=>{
            display.innerText = 
                 (element.value=="0")? "None"
                :(element.value=="1")? "Mails, links, files"
                :(element.value=="2")? "Tags, triggers, sequences"
                :(element.value=="3")? "Subscribers"
                :(element.value=="4")? "Full access"
                :"Value out of range.";
        };
        element.addEventListener("change", updateAccess);
        updateAccess();
    }

    elementValue(element){
        switch(element.tagName){
        case "input": return element.value;
        case "textarea": return element.value;
        case "select": return element.querySelector("option:checked").value;
        default: return null;
        }
    }

    setElementValue(element, value){
        console.log(element, value);
        switch(element.tagName){
        case "input":
        case "textarea": element.value = value; break;
        case "select": [].forEach.call(element.querySelectorAll("option"), (el)=>{
            if(el.getAttribute("value") == value) el.setAttribute("checked", "checked");
        }); break;
        }
    }

    elementId(element){
        if(element.getAttribute("id"))
            return "#"+element.getAttribute("id");
        var id = element.tagName+[].join.call(element.classList, ".");
        var idParent = element.closest("[id]");
        if(idParent) id = "#"+idParent.getAttribute("id")+" "+id;
        if(element.getAttribute("name"))
            id = id + "[name=\""+element.getAttribute("name")+"\"]";
        return id;
    }

    serializeForms(){
        var self = this;
        var data = [];
        [].forEach.call(document.querySelectorAll("input,textarea,select"), (el)=>{
            if(!["hidden","submit","button"].includes(el.getAttribute("type"))){
                data.push({
                    id: self.elementId(el),
                    value: self.elementValue(el)
                });
            }
        });
        return data;
    }

    reloadForms(data){
        var self = this;
        self.log("Restoring form data", data);
        var seen = [];
        data.forEach((entry)=>{
            var element = document.querySelector(entry.id);
            if(element && !seen.includes(element)){
                self.setElementValue(element, entry.value);
                seen.push(element);
            }
        });
    }
}

var courier;
document.addEventListener("DOMContentLoaded", ()=>courier = courier || new Courier());
