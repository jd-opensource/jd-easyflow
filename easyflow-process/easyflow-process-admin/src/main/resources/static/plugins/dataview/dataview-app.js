(function (J) {
J.helperPlugin = function() {
    
    this.helperRender = function($container) {
        if (this.cfg.helper && this.cfg.helper.show===false && window.location.search.indexOf("_pageHelper")<0) {
            return;
        }
        var toolBtnHtml = 
        '<div class="dropdown" style="position:absolute;right:2px;bottom:2px">' +
          '<button class="btn btn-secondary dropdown-toggle" type="button" data-toggle="dropdown"><i class="fas fa-tools"></i></button>' +
          '<div class="dropdown-menu">' +
             (this.cfg.templates && this.cfg.templates.length > 1 ? '<a class="dropdown-item j-view-change" href="#">'+J.msg['dataviewapp.viewChange']+'</a>':'') +
             '<a class="dropdown-item j-data-view" href="#">'+J.msg['dataviewapp.dataView']+'</a>'+
             '<a class="dropdown-item j-data-map" href="#">'+J.msg['dataviewapp.dataMap']+'</a>'+
         '</div>' +
       '</div>';
        var $toolBtn = $(toolBtnHtml).appendTo($container);
        var $viewChangeBtn = $toolBtn.find(".j-view-change");
        var $dataViewBtn = $toolBtn.find(".j-data-view");
        var $mapBtn = $toolBtn.find(".j-data-map");
        var _self = this;

        $mapBtn.click(function(){
           var elementHtml = "";
            var name = "radioName_" + $.jSequence.next();
            var id1 = "radioId_" + $.jSequence.next();
            var checked1=_self.ctx.mapSwitch=="on"?" checked":"";
            elementHtml += '<div class="form-check" style="font-size:16px"><input type="radio" name="' + name + '" id="' + id1 + '" class="form-check-input" value="on"' + checked1 + '><label class="form-check-label" for="' + id1 + '">'+J.msg['dataviewapp.enable']+'</label></div>';
            var id2 = "radioId_" + $.jSequence.next();
            var checked2=_self.ctx.mapSwitch!="on"?" checked":"";
            elementHtml += '<div class="form-check" style="font-size:16px"><input type="radio" name="' + name + '" id="' + id2 + '" class="form-check-input" value="off"' + checked2 + '><label class="form-check-label" for="' + id2 + '">'+J.msg['dataviewapp.disable']+'</label></div>';
            $.jDialog({
                title:J.msg['dataviewapp.dataMapTitle'],
                size: "modal-sm",
                element:elementHtml,
                action:function($dialog){
                    $dialog.find("[name=" + name + "]").click(function(){
                        var mapSwitch = $(this).val();
                        _self.ctx.mapSwitch=mapSwitch;
                        if (mapSwitch == 'on') {
                            _self.$container.find(".dv-tooltip-label").tooltip('enable');
                        } else {
                            _self.$container.find(".dv-tooltip-label").tooltip('disable');
                        }
                        $dialog.modal("hide");
                    });
                }
            });
        });       

        $viewChangeBtn.click(function(){
            var elementHtml = "";
            var name = "radioName_" + $.jSequence.next();
            for (var i in _self.cfg.templates) {
                var templateCode = _self.cfg.templates[i].code;
                var checked = _self.currentTemplateCode == templateCode ? " checked" : "";
                var id = "radioId_" + $.jSequence.next();
                elementHtml += '<div class="form-check" style="font-size:16px"><input type="radio" name="' + name + '" id="' + id + '" class="form-check-input" value="' + templateCode + '"' + checked + '><label class="form-check-label" for="' + id + '">' + _self.cfg.templates[i].name + '</label></div>';
            }
            $.jDialog({
                title:J.msg['dataviewapp.viewChangeTitle'],
                size: "modal-sm",
                element:elementHtml,
                action:function($dialog){
                    $dialog.find("[name=" + name + "]").click(function(){
                        var templateCode = $(this).val();
                        if (templateCode != _self.currentTemplateCode) {
                            _self.loadAndRender(templateCode);
                            $dialog.modal("hide");
                        }
                    });
                }
            });
        });

        $dataViewBtn.click(function(){
            var oldData = _self.originData;
            _self.collect(_self.data);
            var currentData = _self.data;
            
            var dataViewHtml = 
         '<div class="modal" tabindex="-1" role="dialog" id="modal">' + 
           '<div class="modal-dialog  modal-lg" role="document">' + 
            '<div class="modal-content">' + 
              '<div class="modal-header">' + 
                '<h5 class="modal-title">'+J.msg['dataviewapp.dataView']+'</h5>' + 
                '<button type="button" class="close" data-dismiss="modal" aria-label="Close">' + 
                  '<span aria-hidden="true">&times;</span>' + 
                '</button>' + 
              '</div>' + 
              '<div class="modal-body">' + 
                // tab start
              '<ul class="nav nav-tabs" role="tablist">' + 
              '<li class="nav-item" role="presentation">' + 
                '<a class="nav-link active" data-toggle="tab" href="#jCurrentData" role="tab" >'+J.msg['dataviewapp.currentData']+'</a>' + 
              '</li>' + 
              '<li class="nav-item" role="presentation">' + 
                '<a class="nav-link" data-toggle="tab" href="#jOldData" role="tab">'+J.msg['dataviewapp.oldData']+'</a>' + 
              '</li>' + 
            '</ul>' + 
            '<div class="tab-content" id="myTabContent">' + 
              '<div class="tab-pane fade show active" id="jCurrentData" role="tabpanel"><textarea id="currentJsonData" class="form-control" rows="30"></textarea></div>' + 
              '<div class="tab-pane fade" id="jOldData" role="tabpanel"><textarea id="oldJsonData" class="form-control" rows="30" readonly="readonly"></textarea></div>' + 
            '</div>' + 
                // tab end
              '</div>' + 
              '<div class="modal-footer">' + 
              '<button type="button" class="btn btn-primary compare-data">'+J.msg['dataviewapp.dataCompare']+'</button>' + 
              '<button type="button" class="btn btn-primary input-data">'+J.msg['dataviewapp.dataRender']+'</button>' + 
                (_self.cfg.saveAction ? '<button type="button" class="btn btn-primary save-data">'+J.msg['dataviewapp.save']+'</button>':'') + 
                '<button type="button" class="btn btn-secondary" data-dismiss="modal">'+J.msg['dataviewapp.close']+'</button>' + 
              '</div>' +
            '</div>' +
          '</div>' +
        '</div> ';
        var $dialog = $(dataViewHtml).appendTo($("body"));
        $dialog.on("hidden.bs.modal", function(){
            $dialog.modal("dispose");
            $dialog.remove();
          });
       $dialog.on("click", ".j-btn-cancel", function(){
           $dialog.modal("hide");
       });
       $("#currentJsonData").val(JSON.stringify(currentData, null, 2));
       $("#oldJsonData").val(JSON.stringify(oldData, null, 2));
       $(".compare-data").click(function(){
           var compareHtml = 
           '<div class="modal" tabindex="-1" role="dialog" id="compareModal" style="overflow:auto">' +
              '<div class="modal-dialog  modal-lg" role="document">' +
                '<div class="modal-content">' +
                  '<div class="modal-header">' +
                    '<h5 class="modal-title">'+J.msg['dataviewapp.newOldDataCompare']+'</h5>' +
                    '<button type="button" class="close" data-dismiss="modal" aria-label="Close">' +
                      '<span aria-hidden="true">&times;</span>' +
                    '</button>' +
                  '</div>' +
                  '<div class="modal-body">' +
                  '<div style="overflow:auto">' +
                    '<table class="table table-bordered">' +
                        '<thead><tr><th scope="col">'+J.msg['dataviewapp.dataItem']+'</th><th scope="col">'+J.msg['dataviewapp.originalVal']+'</th><th scope="col">'+J.msg['dataviewapp.afterVal']+'</th></tr></thead>' +
                    '<tbody id="compareData"></tbody>' +
                    '</table>' +
                 '</div>' +
                '</div>' +
               '</div>' +
             '</div>' +
          '</div>';
           var $compare = $(compareHtml).appendTo($("body"));
           $compare.on("hidden.bs.modal", function(){
               $compare.modal("dispose");
               $compare.remove();
             });
           $compare.on("click", ".j-btn-cancel", function(){
               $compare.modal("hide");
          }); 
            var result = [];
            var originData = _self.originData;
           var data;
           try {
              data = JSON.parse($("#currentJsonData").val());
           } catch (err) {
               $.jMessage({msg:J.msg['dataviewapp.notJsonFormat']});
               return;
           }
            for (var key in originData) {
                var originVal = JSON.stringify(originData[key], null, 2);
                var newVal = JSON.stringify(data[key], null, 2);
                if (originVal !== newVal) {
                    result.push({key:key, originVal:originVal, newVal:newVal});
                }
            }
            for (var key in data) {
                if (originData[key] === undefined) {
                    var newVal = JSON.stringify(data[key], null, 2);
                    result.push({key:key, originVal:JSON.stringify(undefined), newVal:newVal});
                }
            }
            for (var i in result) {
                $("#compareData").append("<tr><td>" + result[i].key + "</td><td><pre>"+result[i].originVal+"</pre></td><td><pre>"+ result[i].newVal+"</pre></td></tr>");
            }
           $compare.modal();
       });

       $(".input-data").click(function(){
           var data;
           try {
              data = JSON.parse($("#currentJsonData").val());
           } catch (err) {
               $.jMessage({msg:J.msg['dataviewapp.notJsonFormat']});
               return;
           }
           for (var key in _self.cfg.data) {
               delete _self.cfg.data[key];
           }
           $.extend(_self.cfg.data, data);
           if (_self.cfg.allData !== _self.cfg.data) {
                for (var key in _self.cfg.allData) {
                   delete _self.cfg.allData[key];
               }
               $.extend(_self.cfg.allData, data);
           }
           
           if (!_self.cfg.ctx) {
               _self.cfg.ctx = {};
           }

           _self.cfg.ctx.parseValByValProcess=true;
           _self.clear();
           _self.init();
            _self.render(_self.$container);
            $dialog.modal("hide");
       });

       $(".save-data").click(function(){
          var data = $("#currentJsonData").val();
          try {
              JSON.parse(data);
          } catch(err) {
              $.jMessage({msg:J.msg['dataviewapp.notJsonFormat']});
              return;
          }
              _self.cfg.saveAction.call(this, data); 
              $dialog.modal("hide");
       });
       $dialog.modal();
            
        });
    }
    this.loadAndRender = function(template) {
        // Object type
        if (template !== null && !Array.isArray(template) && typeof template === 'object') {
            _renderByConf(this, template);
            return;
        }
        // String type as template code
        var templateCode = template;
        if (! templateCode) {
            if (this.cfg.templates && this.cfg.templates.length>0) {
            templateCode = this.cfg.templates[0].code;
            } else {
                _renderByConf(this, J.jsonTemplateConfig);
                return;
            }
        }
        var _self = this;
        var formTemplateUrl = J.formTemplateUrl ? J.formTemplateUrl :  "/easyflow/formTemplate/ajax/getTemplate";
        $.get($.getBaseUrl() + formTemplateUrl + "?templateCode=" + templateCode, function(result) {
            _self.currentTemplateCode = templateCode;
            template = result.resultData;
            var config = JSON.parse(template.config);
            try {
            _renderByConf(_self, config);
            } catch (error) {
                console.dir(error);
                _renderByConf(_self, J.jsonTemplateConfig);
            }
        });

    }

    function _renderByConf(_self, config) {
        if (_self.rendered) {
            _self.collect(_self.data);
        }
        _self.clear();
        _self.cfg.config = config;
        _self.init();
        _self.render(_self.$container);
    };

};

J.Page = J.Components['page'] = function(cfg) {
    this.cfg = cfg;
    J.helperPlugin.call(this);
    if (!this.cfg.ctx) {
        this.cfg.ctx = {};
    }
    if (this.cfg.ctx.op=="modify" || this.cfg.ctx.op=="detail") {
        this.cfg.ctx.parseValByValProcess = true;
    }
    if (this.cfg.ctx.op=="detail") {
        this.cfg.readonly = true;
    }
    if (cfg.init !== false) {
        this.init();
    }
    this.init = function() {
        var cfg = this.cfg;
        cfg.allConfig = cfg.config;
        cfg.allData = cfg.data;
        if (! cfg.originData) {
            cfg.originData = JSON.parse(JSON.stringify(cfg.data));
        }
        if (cfg.config && cfg.config.nullPolicy == 'IGNORE') {
            cfg.ctx.nullPolicy = 'IGNORE';
        }
        J.BaseComponent.call(this, 'page', cfg);
    }
    this.clear = function() {
        this.$container && this.$container.empty();
        this.component = null;

    }
    this.render = function($container){
         this.$container = $container;
        _beforeRender.call(this, $container);
        var $div = $('<div class="position-relative j-page-container"></div>').appendTo($container);
        if (this.config.topHtml) {
            $div.append(this.config.topHtml);
        }
        var formHtml = '<form class="' + this.componentClass + '" action="javascript:void(0)"></form>';
        this.$form = $(formHtml).appendTo($div);
        this.$form.validate();
        this.component.render(this.$form);
        this.helperRender($div);
        if (this.cfg.readonly) {
            this.$form.find("input").attr('readonly', true);
            this.$form.find("textarea").attr('readonly', true);
            this.$form.find('select').attr('disabled', true);
            this.$form.find("button.add").attr("disabled", true);
            this.$form.find("button.insert").attr("disabled", true);
            this.$form.find("button.remove").attr("disabled", true);
        }
        if (this.config.bottomHtml) {
            $div.append($(this.config.bottomHtml));
        }
        _afterRender.call(this, $container);
        this.rendered = true;
    }

}

J.Tabs = J.Components['tabs'] = function(cfg) {
    J.BaseComponent.call(this, 'tabs', cfg);
    this.render = function($container){
        var tabsHtml = '<ul class="nav nav-tabs" role="tablist"></ul><div class="tab-content"></div>';
        $(tabsHtml).appendTo($container);
        this.renderComponents($container);
        var $tabs = $container.find(".nav-tabs");
        $tabs.children("li").first().children("a").tab("show");
        if (this.components.length==1 && ! this.components[0].cfg.config.name) {
            $tabs.hide();
        }
    }
}
    J.Tab = J.Components['tab'] = function(cfg) {
        J.BaseComponent.call(this, 'tab', cfg);
        this.render = function($container) {
            this.$container = $container;
            var _self = this;
            var $tabs = $container.find(".nav-tabs");
            var $contents = $container.find(".tab-content");
            this.tabId = "tab_" + this.id;
            this.panelId = "panel_" + this.id;
            if (cfg.config.show !== false) {
                var tabHtml = "<li class='nav-item'><a class='nav-link' id='" + this.tabId + "' data-toggle='tab' href='#" + this.panelId + "' role='tab'>" + cfg.config.name + "</a></li>";
                this.$tab = $(tabHtml).appendTo($tabs);
            }
            if (!cfg.config.sider) {
                var contentHtml = "<div class='tab-pane' id='" + this.panelId + "' role='tabpanel'></div>";
                this.$content = $(contentHtml).appendTo($contents);
                this.renderComponents(this.$content);
            } else {
                var contentHtml = "<div class='tab-pane position-relative' id='" + this.panelId + "' role='tabpanel'>" +
                "<div class='sider-control'><i class='far fa-minus-square'></i></div>" +
                "<div class='row'><div class='contentSider'></div>" +
                "<div class='contentContainer col' style='position:relative;height:600px;overflow-y:scroll'></div></div></div>";
                this.$content = $(contentHtml).appendTo($contents);
                this.$siderControl = this.$content.find(".sider-control");
                this.$contentSider = this.$content.find(".contentSider");
                this.$contentContainer = this.$content.find(".contentContainer");
                this.renderComponents(this.$contentContainer);
                this.$siderControl.click(function() {
                    _self.$contentSider.toggle();
                    $(this).find("i").toggleClass("fa-minus-square fa-plus-square");
                });

                var navHtml = '<nav class="nav nav-pills flex-column" style="margin-top:5px">';
                this.components && this.components.forEach(function(element) {
                    var $container = element.$panel || element.$card;
                    if ($container.css("display")!='none') {
                        $container.data("name") && (navHtml+='<a class="nav-link" href="#' + $container.attr("id") + '">' + $container.data("name") + '</a>');
                        if (cfg.config.siderLevel==2) {
                            $container.css("position", "static");
                            navHtml += '<nav class="nav nav-pills flex-column">';
                            element.components && element.components.forEach(function(sub) {
                                var $sub = sub.$panel || sub.$card;
                              if ($sub.css("display")!='none') {
                                $sub.data("name") && (navHtml += '<a class="nav-link ml-3" href="#' + $sub.attr("id") + '">' + $sub.data("name") + '</a>');
                                }
                            });
                            navHtml += "</nav>"
                        }
                    }
                });
                navHtml+="</nav>";
               var $navList = $(navHtml).appendTo(this.$contentSider);
               $navList.find("a").click(function() {
                var offsetTop = document.getElementById($(this).attr("href").substr(1)).offsetTop;
                _self.$contentContainer.scrollTop(offsetTop);
                return false;
            })
            this.$contentContainer.scrollspy({ target: this.$contentSider});
            }
        }
    }

J.Panel = J.Components['panel'] = function(cfg) {
    J.BaseComponent.call(this, 'panel', cfg);
    this.render = function($container){
        this.$container = $container;
        var header = cfg.config.name ?  '<div class="card-header">' +
                '<h4><button class="btn btn-link" type="button" data-toggle="collapse" data-target="#collapse_' + this.id + '">' + cfg.config.name + '</button></h4>' +
             '</div>' : "";
        var panelHtml =
        '<div id="panel_'+ this.id + '" class="j-panel card ' + (cfg.config.show!==false ? "":" d-none") + (cfg.config.name ? "" : "border-0") + '" data-name="' + cfg.config.name + '">' +
            header +
            '<div id="collapse_' + this.id + '" class="collapse show">' +
                '<div class="j-card card-body"></div>' +
            '</div>' +
        '</div>';
        this.$panel = $(panelHtml).appendTo($container);
        this.$cardBody = this.$panel.find(".card-body");
        this.renderComponents(this.$cardBody);
    }
}

J.Card = J.Components['card'] = function(cfg) {
    J.BaseComponent.call(this, 'card', cfg);
    this.render = function($container){
         this.$container = $container;
        var header = this.config.name ? '<div class="card-header">' + this.config.name + '</div>' : '';
        var cardHtml =
        '<div id="card_' + this.id + '" class="j-card card' + (cfg.config.show!==false ? "" : " d-none") + (this.config.name?"":" border-0") + '" data-name="' + cfg.config.name + '">'+
            header +
            '<div class="card-body container-fluid' + (this.config.name?"":" p-0") + '">'+
            '</div>'+
        '</div>';
        this.$card = $(cardHtml).appendTo($container);
        this.$cardBody = this.$card.find(".card-body");
        var cols = this.config.cols;
        if (! cols) {cols=1}
        var $row = null;
        var current = 0;
        for (var i in this.components) {
            if (current % cols == 0) {
                $row = $('<div class="row"></div>').appendTo(this.$cardBody);
            }
            this.components[i].render($row);
            if (this.components[i].config.show !== false) {
                current= this.components[i].config.newline ? 0 : current+1;
            }
        }
    }
}

var _commonCollect = J._commonCollect = function(val, data) {
    if (val == "") {
        val = null;
    }
    var valProcess = this.config.valProcess;
    var cfgType = this.config.cfgType;
    if (cfgType == "json") {
        val = JSON.parse(val);
    }
    var ignoreNull = this.cfg.ctx.nullPolicy == 'IGNORE';
    if (valProcess == "key") {
        if ((val === null || val===undefined) && ignoreNull && data[this.config.cfgKey] === undefined) {
            return;
        }
        data[this.config.cfgKey] = val;
    } else if (valProcess == "keyPath") {
        if ((val === null || val===undefined) && ignoreNull && J.jp.value(data[this.config.cfgKey], this.config.cfgPath) === undefined) {
            return;
        }
        data[this.config.cfgKey] = J.jp.value(data[this.config.cfgKey], this.config.cfgPath, val);
    } else if (valProcess == "self") {
            try {
        var selfFunc = eval(this.config.cfgSelf);
        selfFunc.call(this, "collect");
            } catch (err) {
                console.log("eval Exception, js:" + this.config.cfgSelf);
                console.dir(err);
                throw err;
            }
            
    }
}

var _parseVal = function() {
    var val;
    var parseValByValProcess = this.cfg.ctx.parseValByValProcess;
    if (parseValByValProcess) {
        val = _parseValByValProcess.call(this);
    } else {
        val = this.config.defaultVal;
        if (this.config.valProcess=="self") {
            try {
            var selfFunc = eval(this.config.cfgSelf);
            val = selfFunc.call(this, "parse");
            } catch (err) {
                console.log("eval Exception, js:" + this.config.cfgSelf);
                console.dir(err);
                throw err;
        }
    }
        }
    if (val === undefined) {
        val = null;
    }
    return val;
}
var _parseValByValProcess = function() {
    var val;
    var data = this.allData;
    var valProcess = this.config.valProcess;
    if (! data) return val;
    if (valProcess=="key") {
        val = data[this.config.cfgKey];
    } else if (valProcess=="keyPath") {
        val = J.jp.value(data[this.config.cfgKey], this.config.cfgPath);
    } else if (valProcess=="self") {
            try {
        var selfFunc = eval(this.config.cfgSelf);
        val = selfFunc.call(this, "parse");
            } catch (err) {
                console.log("eval Exception, js:" + this.config.cfgSelf);
                console.dir(err);
                throw err;
            }
    }
    var cfgType = this.config.cfgType;
    if (cfgType == "json") {
        val = JSON.stringify(val,null, 2);
    }
    return val;
}

var has = false;
var _addRules = function() {
    if (this.config.required) {
        this.$element.rules("add", {required:true});
    }
    if (this.config.rule) {
        for (var i in this.config.rule) {
            var rule = this.config.rule[i];
            var ruleInfo = {};
            var ruleMessage = {};
            ruleInfo[rule.type]=rule.value;
            if (rule.message) {
                ruleMessage[rule.type]=rule.message;
                ruleInfo["messages"] = ruleMessage;
            }
            this.$element.rules("add",ruleInfo);
        }
    }
}

var _beforeRender = function($container) {
    var beforeRender = this.config.beforeRender;
        if (beforeRender) {
             try {
                    eval(beforeRender)
                } catch (err) {
                    console.log("eval Exception, js:" + beforeRender);
                    console.dir(err);
                    throw err;
                }            
        }
}
var _afterRender = function($container) {
    var afterRender = this.config.afterRender;
        if (afterRender) {
            try {
                eval(afterRender);
            } catch (err) {
                console.log("eval Exception, js:" + afterRender);
                console.dir(err);
                throw err;
            }

        }
}

var _commonInit = J._commonInit = function() {
    this.render = function($container){
        this.$container = $container;
        _beforeRender.call(this, $container);
        this.col = (this.config.cols && this.config.cols!=-1) ? "col-" + this.config.cols : "col";
        this.show = (this.config.show!==false) ? "":" d-none";
        var inputHtml =
        '<div class="form-group ' + this.col + this.show + '">' +
            '<label class="dv-tooltip-label l_' + this.componentClass + '">' + (this.config.required ? '<span class="j-require">*</span>':'') + this.config.name + ':</label>' +
        '</div>';
        this.$input = $(inputHtml).appendTo($container);
        this.innerRender.call(this, $container);
        this.$label = this.$label ? this.$lable : this.$input.find(".l_" + this.componentClass);
        this.$label.tooltip({"trigger":"click", "title":J.msg['dataviewapp.dataKey'] + this.config.cfgKey + (this.config.cfgPath ? " " + J.msg['dataviewapp.keyPath'] + this.config.cfgPath : "")}).tooltip('disable');
        this.$element = this.$element ? this.$element : this.$input.find("." + this.componentClass);
        if (this.config.desc) {
            this.$element.tooltip({"html":true, "title":this.config.desc});
        }
        if ((!this.ctx || this.ctx.op =='add') && ! this.config.modify0) {
            this.$element.attr("readonly", "readonly");
        }
        if (this.ctx && this.ctx.op =='modify' && ! this.config.modify) {
            this.$element.attr("readonly", "readonly");
        }
        _addRules.call(this);
        if (this.innerVal) {
            this.innerVal.call(this);
        } else {
            var val = _parseVal.call(this);
            this.$element.val(val);
            if (this.afterElementValInit) {
                this.afterElementValInit.call(this);
            }
        }
        _afterRender.call(this, $container);
    }

    this.collect = function(data) {
        _commonCollect.call(this, this.$element.val(), data);
    }
}

J.InputText = J.Components['inputText'] = function(cfg) {
    J.BaseComponent.call(this, 'inputText', cfg);
    var _self = this;
    _commonInit.call(this);
    this.innerRender=function($container){
        $('<input type="text" class="form-control ' + this.componentClass + '" name="' + this.name + '"/>').appendTo(this.$input);
    }
}

J.Select = J.Components['select'] = function(cfg) {
    J.BaseComponent.call(this, 'select', cfg);
    var _self = this;
    _commonInit.call(this);
    this.collect = function(data) {
        var cfgType = this.config.cfgType;
        var val = this.$element.val();
        if (this.config.selectType == 'single') {
            if (val == "") {
                val = null;
            }
            if (cfgType == "json") {
                val = JSON.parse(val);
            }
        } else {
            if (this.config.selectType == 'seqMultiple') {
                val = this.selectVal;
            }
            if (val != null && val.length == 0) {
                val = null;
            }
            if (val != null && cfgType == "json") {
                for (var i in val) {
                    val[i] = JSON.parse(val[i]);
                }
            }
        }
        if (val != null && this.config.selectValType == "comma") {
            if (cfgType == "json") {
                for (var i in val) {
                    val[i] = JSON.stringify(val[i]);
                }
            }
            val = val.join(',');

        }
        var valProcess = this.config.valProcess;
        var ignoreNull = this.cfg.ctx.nullPolicy == 'IGNORE';
        if (valProcess == "key") {
            if ((val === null || val===undefined) && ignoreNull && data[this.config.cfgKey] === undefined) {
                return;
            }
            data[this.config.cfgKey] = val;
        } else if (valProcess == "keyPath") {
            if ((val === null || val===undefined) && ignoreNull && J.jp.value(data[this.config.cfgKey], this.config.cfgPath) === undefined) {
                return;
            }
            data[this.config.cfgKey] = J.jp.value(data[this.config.cfgKey], this.config.cfgPath, val);
        } else if (valProcess == "self") {
                try {
                    var selfFunc = eval(this.config.cfgSelf);
                    selfFunc.call(this, "collect");
                } catch (err) {
                    console.log("eval Exception, js:" + this.config.cfgSelf);
                    console.dir(err);
                    throw err;
                }
            }
    }

    this.refreshSeqSelect = function($element) {
        if (! this.selectVal) this.selectVal = [];
        this.$element.find("option").each(function(index){
            var idx = _self.selectVal.indexOf($(this).val());
            $(this).text((idx<0?'':(idx + 1) + "-") + _self.config.selectList[index].name);
        });
        _self.$element.multiselect("rebuild");
    }
    this.innerRender=function($container){
        $('<select class="form-control ' + this.componentClass + '"' + (cfg.config.selectType=='single'?'':' multiple="multiple"')+ ' name="' + this.name + '"></select>').appendTo(this.$input);
        this.$element = this.$input.find("." + this.componentClass);
        var selectList = this.config.selectList;
        for (var i in selectList) {
            this.$element.append("<option value='"+selectList[i].value+"'>"+selectList[i].name+"</option>");
        }
        if (this.config.selectType == 'multiple') {
            this.$element.multiselect();
        } else if (this.config.selectType == 'seqMultiple') {
            this.$element.multiselect({includeSelectAllOption:false,onChange:function(option, check) {
                _self.selectVal = _self.selectVal || [];
                if (check) {
                    _self.selectVal.push($(option).val());
                } else {
                    _self.selectVal = _self.selectVal.filter(function(item){return item!=$(option).val();});
                }
                _self.refreshSeqSelect(_self.$element);
            }});
        }
    }

    this._checkSelect = function(val) {
       var selectList = this.config.selectList;
        for (var i in selectList) {
            if (selectList[i].value == val) {
                return;
            }
        }
        var error = J.msg['dataviewapp.selectCheck1'] + this.config.name + " "+J.msg['dataviewapp.selectCheck2']+":" + this.config.cfgKey + " "+J.msg['dataviewapp.selectCheck3']+":" + JSON.stringify(this.config.selectList) + " "+J.msg['dataviewapp.selectCheck4']+":" + val;
        console.log(error);
        alert(error);
        throw error;

    }
    this.innerVal = function($container) {
        var val;
        if (this.config.selectType == 'single') {
            val = _parseVal.call(this);
            if (val!==null && val !== "null") {
                this._checkSelect(val);
            }
            this.$element.val(val);
        } else {
            var parseValByValProcess = this.cfg.ctx.parseValByValProcess;
            var valProcess = this.config.valProcess;
            if (parseValByValProcess) {
                var data = this.allData;
                if (! data) return;
                if (valProcess=="key") {
                    val = data[this.config.cfgKey];
                } else if (valProcess=="keyPath") {
                    val = J.jp.value(data[this.config.cfgKey], this.config.cfgPath);
                } else if (valProcess=="self") {
                        try {
                    var selfFunc = eval(this.config.cfgSelf);
                    val = selfFunc.call(this, "parse");
                        } catch (err) {
                            console.log("eval Exception, js:" + this.config.cfgSelf);
                            console.dir(err);
                            throw err;
                        }
                    }
            } else {
                val = this.config.defaultVal && JSON.parse(this.config.defaultVal);
            }
            if (! val) {return;}
            if (this.config.selectValType=='comma') {
                val = val.split(",");
            }
            if (this.config.cfgType == 'json') {
                for (var i in val) {
                    val[i] = JSON.stringify(val[i]);
                }
            }
            for (var i in val) {
                this._checkSelect(val[i]);
            }
            this.$element.val(val);
            if (this.config.selectType == 'seqMultiple') {
                this.selectVal = val;
                this.refreshSeqSelect(_self.$element);
            } else {
                this.$element.multiselect("refresh");
            }
        }
    }
}

J.Textarea = J.Components['textarea'] = function(cfg) {
    J.BaseComponent.call(this, 'textarea', cfg);
    var _self = this;
    _commonInit.call(this);
    this.innerRender=function($container){
        $('<textarea class="form-control ' + this.componentClass + '" name="' + this.name + '"></textarea>').appendTo(this.$input);
    }
}

J.Self = J.Components['self'] = function(cfg) {
    J.BaseComponent.call(this, 'self', cfg);
    var selfScript = this.config.selfScript;
        try {
            this.selfFunc = eval(selfScript);
            this.selfFunc.init(cfg);
        } catch (err) {
            console.log("eval Exception, js:" + selfScript);
            console.dir(err);
            throw err;
        }        

    var _self = this;
    this.render=function($container){
        this.selfFunc.render($container);
    }

    this.collect = function(data) {
       this.selfFunc.collect(data);
    }
}

J.FixInput = J.Components['fixInput'] = function(cfg) {
    J.BaseComponent.call(this, 'fixInput', cfg);
    var _self = this;
    _commonInit.call(this);
    this.innerRender=function($container){
        $('<input type="text" class="form-control ' + this.componentClass + '" name="' + this.name + '" readonly="readonly"/>').appendTo(this.$input);
    }
    this.innerVal = function($container) {
        this.$element.val(this.config.defaultVal);
    }
    this.collect = function(data) {
        // NOOP;
    }
}

J.TableSelect = J.Components['tableSelect'] = function(cfg) {
    J.BaseComponent.call(this, 'tableSelect', cfg);
    var _self = this;
    _commonInit.call(this);
    this.collect = function(data) {
        var val = this.$tableSelect.data("jTableSelect").$valueField.val();
        _commonCollect.call(this, val, data);
    }

    this.innerRender=function($container){
        $('<div id="tableSelect_' + this.id + '" class="' + this.componentClass + '"></div>').appendTo(this.$input);
        this.$tableSelect = this.$input.find("." + this.componentClass);
        var val = _parseVal.call(this);
        var conf = this.tableSelectConf;// computed from beforeRender
        conf.initValue = val;
        conf.name="elName_" + this.id;
        this.$tableSelect.jTableSelect(conf);
        this.$element=this.$tableSelect.data("jTableSelect").$valueField;
    }
    this.innerVal = function($container) {}
}

J.ListSelect = J.Components['listSelect'] = function(cfg) {
    J.BaseComponent.call(this, 'listSelect', cfg);
    var _self = this;
    _commonInit.call(this);
    this.collect = function(data) {
        var val = this.$listSelect.data("jListSelect").$valueField.val();
        _commonCollect.call(this, val, data);
    }

    this.innerRender=function($container){
        $('<div id="listSelect_' + this.id + '" class="' + this.componentClass + '"></div>').appendTo(this.$input);
        this.$listSelect = this.$input.find("." + this.componentClass);
        var val = _parseVal.call(this);
        var conf = this.listSelectConf;// computed from beforeRender
        conf.initValue = val;
        conf.name="elName_" + this.id;
        this.$listSelect.jListSelect(conf);
        this.$element=this.$listSelect.data("jListSelect").$valueField;
    }
    this.innerVal = function($container) {}
}

J.CardList = J.Components['cardList'] = function(cfg) {
    J.List.call(this, cfg);
    var _listRender = this.render;
    this.render = function($container) {
        _beforeRender.call(this, $container);
        var dataStr = _parseValByValProcess.call(this);
        if (dataStr) {
            this.data = JSON.parse(dataStr);
        }
        _listRender.call(this, $container);
        this.col = (this.config.cols && this.config.cols!=-1) ? "col-" + this.config.cols : "col";
        this.$list.addClass(this.col);
        _afterRender.call(this, $container);
    }
    var _listCollect = this.collect;
    this.collect = function(data) {
        var newListData = _listCollect.call(this, data);
        _commonCollect.call(this, JSON.stringify(newListData), data);
    }
}

    J.Table = J.Components['table'] = function(cfg) {
        J.BaseComponent.call(this, 'table', cfg);
        var _self = this;
        _commonInit.call(this);
        this.innerRender=function($container){
            $('<input type="hidden" class="form-control ' + this.componentClass + '" name="' + this.name + '"></input>').appendTo(this.$input);
            this.$table = $("<table id='#" + this.id + "'></table>").appendTo(this.$input);
            var conf = this.tableConf;//computed from beforeRender
            this.$table.bootstrapTable(conf);
        }
        this.afterElementValInit = function() {
            var conf = this.tableConf;//computed from beforeRender
            var val = this.$element.val();
            if (val) {
                conf.data = JSON.parse(this.$element.val());
            }
            this.$table.bootstrapTable("destroy");
            this.$table.bootstrapTable(conf);
        }
    }

   })(window.J);
