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
    }

    apiCall(endpoint, args){
        var self = this;
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData = new FormData();

            formData.append("data-format", "json");
            for(var field in args){
                formData.append(field, args[field]);
            }
            request.onload = ()=>{
                var data = JSON.parse(request.responseText);
                if(request.status === 200 && data.status == 200){
                    self.log("Request succeeded", data.data);
                    ok(data.data);
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
                .then((data)=>{self.cache[type] = data; return data;});
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

    registerAll(element, regger){
        var self = this;
        var elements = document.querySelectorAll(".type-select");
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
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
}

var courier = new Courier();
