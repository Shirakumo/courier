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

        self.registerAll(".type-select", self.registerTypeSelect);
        self.registerAll(".button.confirm", self.registerConfirm);
        self.registerAll(".editor", self.registerEditor);
        self.registerAll(".chart", self.registerChart);
        self.registerAll("form", self.registerForm);
        self.registerAll(".dynamic-list", self.registerDynamicList);
        self.registerAll(".tag-list", self.registerTags);
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

    registerForm(element){
        var self = this;
        var save = element.querySelector("input[type=submit]");
        if(!save) return;
        save.addEventListener("click", (ev)=>{
            ev.preventDefault();
            if(element.checkValidity()){
                self.apiCall(save.getAttribute("formaction") || element.getAttribute("action"), element)
                    .then((r)=>{window.location.replace(r.target);},
                          (r)=>{document.querySelector(".box.error").innerText = r.message;});
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

    registerDynamicList(element){
        var self = this;
        var reg = (element)=>{
            element.querySelector(".remove-self").addEventListener("click",()=>{
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
                        .then((r)=>preview.src = "data:text/html;charset=utf-8,"+escape(r),
                              (r)=>preview.src = "data:text/html;charset=utf-8,"+escape("Preview failed: "+r.message));
                    preview.classList.remove("hidden");
                    element.querySelector(".CodeMirror").classList.add("hidden");
                }else{
                    element.querySelector(".CodeMirror").classList.remove("hidden");
                    preview.classList.add("hidden");
                }
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
            .then(()=>{
                chart = new Chart(ctx, {
                    type: element.dataset.type,
                    data: {
                        datasets: [{
                            data: [],
                            backgroundColor: 'rgba(245,166,49,0.2)',
                            borderColor: 'rgba(245,166,49,1)',
                            pointRadius: 10
                        }],
                        labels: []
                    },
                    options: {
                        maintainAspectRatio: false,
                        layout: {
                            padding: 5
                        },
                        legend: {
                            display: false
                        },
                        scales: {
                            yAxes: [{
                                ticks: {
                                    suggestedMin: 0
                                }
                            }]
                        }
                    }
                });
                if(element.dataset.type == 'doughnut'){
                    chart.data.datasets[0].backgroundColor = ["rgb(255, 205, 86)","rgb(255, 99, 132)","rgb(54, 162, 235)"];
                    delete chart.data.datasets[0].borderColor;
                }
                refresh();
            });
        [].forEach.call(element.querySelectorAll("select"), (el)=>{
            el.addEventListener("change", refresh);
        });
    }
}

var courier;
document.addEventListener("DOMContentLoaded", ()=>courier = courier || new Courier());
