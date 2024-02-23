(function(J) {
    /**
     * Context pad
     */
    class EasyFlowContextPad {
        constructor(config, contextPad, create, elementFactory, injector, translate) {
            this.create = create;
            this.elementFactory = elementFactory;
            this.translate = translate;

            if (config.autoPlace !== false) {
                this.autoPlace = injector.get('autoPlace', false);
            }

            contextPad.registerProvider(this);
        }

        getContextPadEntries(element) {
            const {
                autoPlace,
                create,
                elementFactory,
                translate
            } = this;

            function appendScriptTask(event, element) {
                if (autoPlace) {
                    const shape = elementFactory.createShape({ type: 'bpmn:ScriptTask' });

                    autoPlace.append(element, shape);
                } else {
                    appendScriptTaskStart(event, element);
                }
            }

            function appendScriptTaskStart(event) {
                const shape = elementFactory.createShape({ type: 'bpmn:ScriptTask' });

                create.start(event, shape, element);
            }

            function appendIntermediateCatchEvent(event, element) {
                if (autoPlace) {
                    const shape = elementFactory.createShape({ type: 'bpmn:IntermediateCatchEvent', eventDefinitionType: 'bpmn:MessageEventDefinition' });

                    autoPlace.append(element, shape);
                } else {
                    appendIntermediateCatchEventStart(event, element);
                }
            }

            function appendIntermediateCatchEventStart(event) {
                const shape = elementFactory.createShape({ type: 'bpmn:IntermediateCatchEvent' });
                create.start(event, shape, element);
            }

            return function(entries) {
                if (entries['append.append-task']) {
                    entries['append.append-task'].action.click = appendScriptTask;
                    entries['append.append-task'].action.dragstart = appendScriptTaskStart;
                }
                if (entries['append.intermediate-event']) {
                    entries['append.intermediate-event'].action.click = appendIntermediateCatchEvent;
                    entries['append.intermediate-event'].action.dragstart = appendIntermediateCatchEventStart;
                }

                return entries;
            }
        }
    }

    EasyFlowContextPad.$inject = [
        'config',
        'contextPad',
        'create',
        'elementFactory',
        'injector',
        'translate'
    ];
    class EasyFlowPalette {
        constructor(create, elementFactory, palette, translate) {
            this.create = create;
            this.elementFactory = elementFactory;
            this.translate = translate;

            palette.registerProvider(this);
        }

        getPaletteEntries(element) {
            const {
                create,
                elementFactory,
                translate
            } = this;

            function createScriptTask(event) {
                const shape = elementFactory.createShape({ type: 'bpmn:ScriptTask' });

                create.start(event, shape);
            }


            return function(entries) {
                var newEntries = {
                    "create.start-event": entries['create.start-event'],
                    'create.script-task': {
                        group: 'activity',
                        className: 'bpmn-icon-script-task',
                        title: translate('Create ScriptTask'),
                        action: {
                            dragstart: createScriptTask,
                            click: createScriptTask
                        }
                    },
                    'create.exclusive-gateway': entries['create.exclusive-gateway'],
                    "create.data-store": entries['create.data-store'],
                    "create.participant-expanded": entries['create.participant-expanded'],
                    'create.group': entries['create.group'],
                    "lasso-tool": entries['lasso-tool'],
                }

                return newEntries;
            }
        }


    }

    EasyFlowPalette.$inject = [
        'create',
        'elementFactory',
        'palette',
        'translate'
    ];

    /**
     * Dropdown option
     */
    function EasyFlowReplaceMenuProvider(
        bpmnFactory, popupMenu, modeling, moddle,
        bpmnReplace, rules, translate) {

        this._bpmnFactory = bpmnFactory;
        this._popupMenu = popupMenu;
        this._modeling = modeling;
        this._moddle = moddle;
        this._bpmnReplace = bpmnReplace;
        this._rules = rules;
        this._translate = translate;

        this.register = function() {
            this._popupMenu.registerProvider('bpmn-replace', this);
        }

        this.getPopupMenuEntries = function(element) {
            return function(entries) {
                return entries;
            }
        }
        this.getPopupMenuHeaderEntries = function(element) {
            return function(entries) {
                return entries;
            }
        };
        this.register();
    }

    EasyFlowReplaceMenuProvider.$inject = [
        'bpmnFactory',
        'popupMenu',
        'modeling',
        'moddle',
        'bpmnReplace',
        'rules',
        'translate'
    ];

    /**
     * Self define module
     */
    var easyFlowModule = {
        __init__: ['easyFlowContextPad', 'easyFlowPalette', 'easyFlowReplaceMenuProvider'],
        easyFlowContextPad: ['type', EasyFlowContextPad],
        easyFlowPalette: ['type', EasyFlowPalette],
        easyFlowReplaceMenuProvider: ['type', EasyFlowReplaceMenuProvider],
        translate: ['value', easyFlowTranslate],
        "config.zoomScroll": ['value', { enabled: false }]
    }

    var easyFlowViewModule = {
        translate: ['value', easyFlowTranslate],
        "config.zoomScroll": ['value', { enabled: false }]
    }

    /**
     * Create BPMN Control
     */
    J.BpmnControl = function(cfg) {
        this.init = function(cfg) {
            this.cfg = cfg;
            this.data = cfg.data ? cfg.data : {};
            this.$bpmnContainer = cfg.$bpmnContainer;
            this.originData = JSON.parse(JSON.stringify(this.data));
            return this;
        }
        this.render = async function($bpmnContainer) {
            var _self = this;
            if ($bpmnContainer) { this.$bpmnContainer = $bpmnContainer };
            $bpmnContainer = this.$bpmnContainer;
            $bpmnContainer.append(this._html());
            // Render control
            var config = {
                container: $bpmnContainer.find(".j-flow-canvas"),
                keyboar: {
                    bindTo: window
                },
                moddleExtensions: {
                    easyflow: easyflowExtension
                },
                additionalModules: cfg.mode == 'view' ? [easyFlowViewModule] : [easyFlowModule]
            };
            this.bpmnModeler = this.cfg.mode == 'view' ? new BpmnNavigatedViewer(config) : new BpmnModeler(config);
            // shrink
            this.$bpmnContainer.find(".j-bpmn-zoomin").click(function() {
                const zoomScroll = _self.bpmnModeler.get('zoomScroll');
                zoomScroll.stepZoom(-1);
            });
            // enlarge
            this.$bpmnContainer.find(".j-bpmn-zoomout").click(function() {
                const zoomScroll = _self.bpmnModeler.get('zoomScroll');
                zoomScroll.stepZoom(1);
            });
            // svg download
            this.$bpmnContainer.find(".j-bpmn-svgdownload").click(function() {
                _self.downloadSvg();
            });
            // fullscreen
            this.$bpmnContainer.find(".j-bpmn-fullscreen").click(function() {
                _self.$bpmnContainer.find(".j-bpmn-pannel").toggleClass("j-bpmn-pannel-normal j-bpmn-pannel-fullscreen");
                $(this).find("i").toggleClass("fa-expand fa-compress");
            });
            // import
            this.$bpmnContainer.find(".j-bpmn-import").click(function() {
                $(this).find("i").toggleClass("fa-info");
                _self.import();
            });
            // export
            this.$bpmnContainer.find(".j-bpmn-export").click(function() {
                _self.export();
            });
            // comment
            this.$bpmnContainer.find(".j-bpmn-comment-select").multiselect({nonSelectedText:J.msg["bpmn.detailInfoType"]});
            this.$bpmnContainer.find(".j-bpmn-comment-select").change(function() {
                _self._commentAll();
            });
            // min pannel
            this.$bpmnContainer.find(".j-bpmn-info-switch").click(function() {
                _self.$bpmnContainer.find(".j-bpmn-info-title").toggle();
                _self.$bpmnContainer.find(".infoPannel>form").toggle();
                _self.$bpmnContainer.find(".infoPannel").toggleClass("infoPannel-up");
            })
            if (this.cfg.mode == 'view') {
                _self.$bpmnContainer.find(".j-bpmn-info-title").toggle();
                _self.$bpmnContainer.find(".infoPannel>form").toggle();
                _self.$bpmnContainer.find(".infoPannel").toggleClass("infoPannel-up");
            }
            // event bind
            var eventBus = this.bpmnModeler.get('eventBus');

            var $infoPannel = this.$bpmnContainer.find(".infoPannel");
            var $form = $infoPannel.find('form');
            $form.validate();
            eventBus.on("element.click", function(e) {
                var elementType = e.element.businessObject.$type;
                console.log('click on ' + e.element.id + " type:" + e.element.businessObject.$type);
                $form.empty();
                if (_self._elementPannelRender[elementType]) {
                    _self._elementPannelRender[elementType].call(_self, $form, e.element);
                }
            });
            eventBus.on("selection.changed", function(e) {
                var selectedElements = e.newSelection;
                if (!selectedElements || selectedElements.length!=1) {
                    $form.empty();
                    return;
                }
                var element = selectedElements[0];
                var elementType = element.businessObject.$type;
                console.log('click on ' + element.id + " type:" + element.businessObject.$type);
                $form.empty();
                if (_self._elementPannelRender[elementType]) {
                    _self._elementPannelRender[elementType].call(_self, $form, element);
                }
            });            
            eventBus.on("element.changed", function(e) {
                console.log("change");
                _self._comment(e.element);
            });
            // BPMN Definition View Pannel
            this.$bpmnContainer.find(".j-bpmn-view").click(function() {
                _self.exportDiagram().then(function(result) {
                    var elementHtml = _self._bpmnViewHtml();
                    var bpmnXml = result.xml;
                    $.jDialog({
                        title: J.msg['bpmn.bpmnFlowDefinition'],
                        size: "modal-lg",
                        element: elementHtml,
                        action: function($dialog) {
                            $dialog.find(".bpmnDef").val(bpmnXml);
                            // bind button action
                            _self._bindDialogButtonAction($dialog);
                        }
                    });
                });
            });            
            // open flow definition
            var diagram = this.data.bpmnXmlData ? this.data.bpmnXmlData : this.initialDiagram;
            await this.openDiagram(diagram);
        }
        this.collect = async function() {
            // JSON data has been set
            this.data.bpmnXmlData = (await this.exportDiagram()).xml;
            return this.data.bpmnXmlData;
        }
        this.init(cfg);
        return this;
    }

    /**
     * Bind dialog action
     */
    J.BpmnControl.prototype._bindDialogButtonAction = function($dialog) {
        var _self = this;
        // render button
        $dialog.find(".j-btn-bpmn-render").click(function() {
            var bpmnXml = $dialog.find(".bpmnDef").val();
            _self.openDiagram(bpmnXml).then(function() {
                $dialog.modal("hide");
            });
        });
        // view JSON button
        $dialog.find(".j-btn-bpmn-json").click(function() {
            var bpmnXml = $dialog.find(".bpmnDef").val();
            var bpmn2JsonUrl = window.bpmn2JsonUrl ? window.bpmn2JsonUrl : ($.getBaseUrl() && $.getBaseUrl().indexOf("http")==0 ? $.getBaseUrl() + "/public/ajax/bpmn2Json": null);
            $.post(bpmn2JsonUrl, { bpmnXmlData: bpmnXml }, function(result) {
                // error tip
                if (result.resultMsg) {
                    $.jMessage({ msg: result.resultMsg });
                    return;
                }
                // JSON content show
                var html =
                    '<div class="row">        ' +
    '<div class="form-group col"> ' +
        '<label><span class="j-require">*</span>' + J.msg['bpmn.flowJsonDefinition'] + ':</label>' +
        '<textarea class="jsonDef form-control" rows="30" readonly="readonly"></textarea>' +
    '</div>' +
'</div>';
                $.jDialog({
                    "title": J.msg["bpmn.jsonContent"], element: html, action: function($dialog) {
                        $dialog.find("textarea").val(result.resultData);
                    }
                })
            }, "json").fail(function(){$.jMessage({ msg: J.msg['bpmn.convertErrorTip'] + bpmn2JsonUrl });});
        });
        // BPMN Compare
        $dialog.find(".j-btn-compare-bpmn").click(function() {
            var newVal = $dialog.find(".bpmnDef").val();
            var oldVal = _self.originData.bpmnXmlData;
            $.jDiffDialog({ left: { title: J.msg['bpmn.newFlowDefinition'], content: newVal }, right: { title: J.msg['bpmn.oldFlowDefinition'], content: oldVal } });
        });
        // EasyFlow Compare
        $dialog.find(".j-btn-compare-easyflow").click(function() {
            var oldVal = _self.originData.jsonData;
            var bpmnXml = $dialog.find(".bpmnDef").val();
            var bpmn2JsonUrl = window.bpmn2JsonUrl ? window.bpmn2JsonUrl : $.getBaseUrl() + "/public/ajax/bpmn2Json";
            $.post(bpmn2JsonUrl, { bpmnXmlData: bpmnXml }, function(result) {
                // Error tip
                if (result.resultMsg) {
                    $.jMessage({ msg: result.resultMsg });
                    return;
                }
                $.jDiffDialog({ left: { title: J.msg['bpmn.newFlowDefinition'], content: result.resultData }, right: { title: J.msg['bpmn.oldFlowDefinition'], content: oldVal } });
            });
        });
    }

    /**
     *Open BPMN
      */
    J.BpmnControl.prototype.openDiagram = async function(bpmnXML) {
        // import diagram
        var bpmnModeler = this.bpmnModeler;
        await bpmnModeler.importXML(bpmnXML);
        // access modeler components
        var canvas = bpmnModeler.get('canvas');
        var overlays = bpmnModeler.get('overlays');
        // zoom to fit full viewport
        canvas.zoom('fit-viewport');
        this._initPropertiesPannel();
        this._commentAll();
        var processElements = _findProcessElement(bpmnModeler);
        var callback = this.cfg.openDiagramCallBack;
        callback && callback.call(this, processElements);
    }

    /**
     * Export BPMN
     */
    J.BpmnControl.prototype.exportDiagram = async function() {
        var result = await this.bpmnModeler.saveXML({
            format: true
        });
        console.log('DIAGRAM', result.xml);
        return result;
    }

    /**
     * Download SVG
     */
    J.BpmnControl.prototype.downloadSvg = async function() {
        var _self = this;
        this.bpmnModeler.saveSVG({ format: true }).then(function(obj) {
            var svg = obj.svg;
            var encodedData = encodeURIComponent(svg);
            var processId = _findProcessElement(_self.bpmnModeler)[0].businessObject.id;
            var fileName = processId + "-" + J.getNowDatetime("", "-", "") + '.svg';
            var downloadLink = document.createElement('a');
            downloadLink.download = fileName;
            downloadLink.innerHTML = 'Get BPMN SVG';
            downloadLink.href = 'data:application/bpmn20-xml;charset=UTF-8,' + encodedData;
            downloadLink.onclick = function(event) {
                document.body.removeChild(event.target);
            };
            downloadLink.style.visibility = 'hidden';
            document.body.appendChild(downloadLink);
            downloadLink.click();
        });
    }

    /**
    * Import
    */
    J.BpmnControl.prototype.import = function() {
        var _self = this;
        $(`<input type="file" value="SelectFile"></input>`)
            .click()
            .on('change', event => {
                let file = event.target.files[0];
                let file_reader = new FileReader();
                file_reader.onload = () => {
                    let fc = file_reader.result;
                    _self.openDiagram(fc);
                };
                file_reader.readAsText(file, 'UTF-8');
            });
    }

    /**
     * Export
     */
    J.BpmnControl.prototype.export = async function() {
        var _self = this;
        this.bpmnModeler.saveXML({ format: true }).then(function(obj) {
            var xml = obj.xml;
            var encodedData = encodeURIComponent(xml);
            var processId = _findProcessElement(_self.bpmnModeler).businessObject.id;
            var fileName = processId + "-" + J.getNowDatetime("", "-", "") + '.bpmn';
            var downloadLink = document.createElement('a');
            downloadLink.download = fileName;
            downloadLink.innerHTML = 'Get BPMN XML';
            downloadLink.href = 'data:application/bpmn20-xml;charset=UTF-8,' + encodedData;
            downloadLink.onclick = function(event) {
                document.body.removeChild(event.target);
            };
            downloadLink.style.visibility = 'hidden';
            document.body.appendChild(downloadLink);
            downloadLink.click();
        });
    }
    /**
    *Init info pannel
    */
    J.BpmnControl.prototype._initPropertiesPannel = function() {
        // Show flow definition pannel
        var processElements = _findProcessElement(this.bpmnModeler);
        if (processElements.length != 1) {
            var $infoPannel = this.$bpmnContainer.find(".infoPannel");
            var $form = $infoPannel.find('form');
             $form.empty();
            return;
        }
        this._elementPannelRender['bpmn:Process'].call(this, this.$bpmnContainer.find(".infoPannel").find("form"), processElements[0]);
    }

    J.BpmnControl.prototype._elementPannelRender = {};
    /**
     * Basic element render
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:Element"] = function($infoPannel, element, hasProperties) {
        this.$bpmnContainer.find(".infoPannel").find("form").empty();
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // ID
        var elementId = bo.id;
        var elementIdHtml = '<div class="row">' +
            '<div class="form-group col"><span class="j-require">*</span><label>ID:</label> <input type="text" class="form-control j-elementId" name="j-elementId" value="' + elementId + '"/></div>'
            + '</div>';
        var $elementIdElement = $(elementIdHtml).appendTo($infoPannel);
        var $elementId = $elementIdElement.find(".j-elementId");
        $elementId.tooltip({ title: J.msg['bpmn.idTooltip'] });
        $elementId.rules('add', { required: true });
        $elementId.blur(function() {
            var newElementId = $elementId.val();
            if (!newElementId) {
                alert(J.msg['bpmn.idEmptyError']);
                $elementId.val(elementId);
                return;
            }
            var elementRegistry = _self.bpmnModeler.get('elementRegistry');
            var exists = elementRegistry.find(function(element){
                return element.businessObject.id==newElementId;
            });
            if (exists) {
                alert(J.msg['bpmn.idExistsError']);
                $elementId.val(elementId);
                return; 
            }
            
            
            bo.id = newElementId;
            _self.cfg.onBpmnDefinitionChange && _self.cfg.onBpmnDefinitionChange.call(_self, bo, "id", newElementId, elementId);
            _self._comment(element);
        });
        // Name
        var elementName = bo.name;
        var elementNameHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.name'] + ':</label> <input type="text" class="form-control j-elementName" name="j-elementName" value="' + (elementName ? elementName : '') + '"/></div>'
            + '</div>';
        var $elementNameElement = $(elementNameHtml).appendTo($infoPannel);
        var $elementName = $elementNameElement.find(".j-elementName");
        $elementName.tooltip({ title: J.msg['bpmn.nameTooltip'] })
        $elementName.blur(function() {
            var newElementName = $elementName.val();
            bo.name = newElementName;
            _self.cfg.onBpmnDefinitionChange && _self.cfg.onBpmnDefinitionChange.call(_self, bo, "name", newElementName, elementName);
            eventBus.fire("element.changed", { element: element });
        });
        // Documentation
        var documentation = bo.documentation ? bo.documentation[0].text : '';
        var documentationHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.documentation'] + ':</label> <textarea class="form-control j-documentation" name="j-documentation"></textarea></div>'
            + '</div>';
        var $documentationElement = $(documentationHtml).appendTo($infoPannel);
        var $documentation = $documentationElement.find(".j-documentation");
        $documentation.tooltip({ title: J.msg['bpmn.documentationTooltip'] })
        $documentation.text(documentation);
        $documentation.blur(function() {
            var newDocumentation = $documentation.val();
            if (!newDocumentation) {
                delete bo.documentation;
            } else {
                if (!bo.documentation) {
                    bo.documentation = [moddle.create('bpmn:Documentation')];
                }
                bo.documentation[0].text = newDocumentation;
            }
        });
        
        // Property
        if (hasProperties === false) {
            return;
        }
        var properties = getExtensionBody(bo, "easyflow:Properties");
        var propertiesHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.property'] + '(<button type="button" class="j-properties-mode btn btn-link p-0">' + J.msg['bpmn.switchJsonMode'] + '</button>):</label> <textarea class="form-control j-properties" name="j-properties"></textarea></div>'
            + '</div>';
        var $propertiesElement = $(propertiesHtml).appendTo($infoPannel);
        var $properties = $propertiesElement.find(".j-properties");
        $properties.tooltip({ title: J.msg['bpmn.propertyTooltip'] });
        $properties.rules('add', { json: true });
        $properties.text(properties);
        $properties.blur(function() {
            var newProperties = $properties.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Properties", newProperties);
            _self._comment(element);
        });
        
        $properties.jJsonPropertyEditor({mode:"ROW"});
        var $propertiesMode = $propertiesElement.find(".j-properties-mode");
        $propertiesMode.click(function(){
            if (!$(this).data('j-properties-mode') || $(this).data('j-properties-mode')=='ROW') {
                $(this).text(J.msg['bpmn.switchRowMode']);
                $properties.jJsonPropertyEditor("changeMode", "JSON");
                $(this).data('j-properties-mode', "JSON");
            } else {
                $(this).text(J.msg['bpmn.switchJsonMode']);
                $properties.jJsonPropertyEditor("changeMode", "ROW");     
                $(this).data('j-properties-mode', "ROW");           
            }
        });

    }

    /**
     * Condition type
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:ExtConditionType"] = function($infoPannel, element) {
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // Property
        var conditionType = getExtensionBody(bo, "easyflow:ConditionType");
        var conditionTypeHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg["bpmn.conditionType"] + ':</label> ' +
            '<select class="form-control j-bpmn-conditionType" name="j-conditionType">' +
                '<option value="">' + J.msg['bpmn.conditionType.inclusive'] + '</option>' +
                '<option value="exclusive">' + J.msg['bpmn.conditionType.exclusive'] + '</option>' +
            '</select>' +
            '</div>' +
           '</div>';
        var $conditionTypeElement = $(conditionTypeHtml).appendTo($infoPannel);
        var $conditionType = $conditionTypeElement.find(".j-bpmn-conditionType");
        $conditionType.tooltip({ title: J.msg['bpmn.conditionTypeTooltip'] });
        $conditionType.val(conditionType);
        $conditionType.blur(function() {
            var newConditionType = $conditionType.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:ConditionType", newConditionType);
            _self._comment(element);
        });

    }
    
     /**
     * Start node
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:NodeStart"] = function($infoPannel, element) {
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // Property
        var start = getExtensionBody(bo, "easyflow:Start");
        var startHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.startNode'] + ':</label> ' +
            '<select class="form-control j-bpmn-start" name="j-start">' +
                '<option value="">' + J.msg['bpmn.startNode.false'] + '</option>' +
                '<option value="true">' + J.msg['bpmn.startNode.true'] + '</option>' +
            '</select>' +
            '</div>' +
           '</div>';
        var $startElement = $(startHtml).appendTo($infoPannel);
        var $start = $startElement.find(".j-bpmn-start");
        $start.tooltip({ title: J.msg['bpmn.startNodeTooltip'] });
        $start.val(start);
        $start.blur(function() {
            var newStart = $start.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Start", newStart);
            _self._comment(element);
        });

    }
    
    /**
     * Pre
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:NodePre"] = function($infoPannel, element) {
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // Property
        var pre = getExtensionBody(bo, "easyflow:Pre");
        var preHtml = '<div class="row"><div class="form-group col"><label>' + J.msg['bpmn.selfPre'] + ':</label> <textarea class="form-control j-bpmn-pre" name="j-bpmn-pre"></textarea></div></div>';
        var $preElement = $(preHtml).appendTo($infoPannel);
        var $pre = $preElement.find(".j-bpmn-pre");
        $pre.tooltip({ title: J.msg['bpmn.selfPreTooltip'] });
        $pre.val(pre);
        $pre.blur(function() {
            var newPre = $pre.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Pre", newPre);
            _self._comment(element);
        });

    }
    
    /**
     * Action
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:NodeAction"] = function($infoPannel, element) {
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // Property
        var action = getExtensionBody(bo, "easyflow:Action");
        var actionHtml = '<div class="row"><div class="form-group col"><label>' + J.msg['bpmn.selfAction'] + ':</label> <textarea class="form-control j-bpmn-action" name="j-bpmn-action"></textarea></div></div>';
        var $actionElement = $(actionHtml).appendTo($infoPannel);
        var $action = $actionElement.find(".j-bpmn-action");
        $action.tooltip({ title: J.msg['bpmn.selfActionTooltip'] });
        $action.val(action);
        $action.blur(function() {
            var newAction = $action.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Action", newAction);
            _self._comment(element);
        });

    }
    
    /**
     * Post
     */
    J.BpmnControl.prototype._elementPannelRender["bpmn:NodePost"] = function($infoPannel, element) {
        const moddle = this.bpmnModeler.get('moddle');
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        var _self = this;
        // Property
        var post = getExtensionBody(bo, "easyflow:Post");
        var postHtml = '<div class="row"><div class="form-group col"><label>' + J.msg['bpmn.selfPost'] + ':</label> <textarea class="form-control j-bpmn-post" name="j-bpmn-post"></textarea></div></div>';
        var $postElement = $(postHtml).appendTo($infoPannel);
        var $post = $postElement.find(".j-bpmn-post");
        $post.tooltip({ title: J.msg['bpmn.selfPostTooltip'] });
        $post.val(post);
        $post.blur(function() {
            var newPost = $post.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Post", newPost);
            _self._comment(element);
        });

    }

    // Collaboration
    J.BpmnControl.prototype._elementPannelRender["bpmn:Collaboration"] = function($infoPannel, element) {
        var participants = element.businessObject.participants;
        if (participants && participants.length == 1) {
          var processBusinessObject= participants[0].processRef;
          if (! processBusinessObject) {
              return;
          }
          var processElement = {};
          processElement.businessObject = processBusinessObject;
          this._elementPannelRender["bpmn:Process"].call(this, $infoPannel, processElement);
        }
    }
    
    // Participant
    J.BpmnControl.prototype._elementPannelRender["bpmn:Participant"] = function($infoPannel, element) {
        var processBusinessObject = element.businessObject.processRef;
        if (! processBusinessObject) {
            return;
        }
        var processElement = {};
        processElement.businessObject = processBusinessObject;
        this._elementPannelRender["bpmn:Process"].call(this, $infoPannel, processElement);
    }

    // Lane
    J.BpmnControl.prototype._elementPannelRender["bpmn:Lane"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Participant"].call(this, $infoPannel, element.parent);
    }    

    // Flow
    J.BpmnControl.prototype._elementPannelRender["bpmn:Process"] = function($infoPannel, element) {
        var _self = this;
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        var bo = element.businessObject;
        var eventBus = this.bpmnModeler.get('eventBus');
        // Flow pre handler
        var flowPreHandler = getExtensionBody(bo, "easyflow:Pre");
        var flowPreHandlerHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.flowPreHandler'] + ':</label> <textarea class="form-control j-bpmn-flowprehandler" name="j-bpmn-flowprehandler"></textarea></div>'
            + '</div>';
        var $flowPreHandlerElement = $(flowPreHandlerHtml).appendTo($infoPannel);
        var $flowPreHandler = $flowPreHandlerElement.find(".j-bpmn-flowprehandler");
        $flowPreHandler.tooltip({ title: J.msg['bpmn.flowPreHandlerTooltip'] })
        $flowPreHandler.rules('add', { json: true });
        $flowPreHandler.text(flowPreHandler);
        $flowPreHandler.blur(function() {
            var newFlowPreHandler = $flowPreHandler.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Pre", newFlowPreHandler);
        });
        // Flow post handler
        var flowPostHandler = getExtensionBody(bo, "easyflow:Post");
        var flowPostHandlerHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.flowPostHandler'] + ':</label> <textarea class="form-control j-bpmn-flowposthandler" name="j-bpmn-flowposthandler"></textarea></div>'
            + '</div>';
        var $flowPostHandlerElement = $(flowPostHandlerHtml).appendTo($infoPannel);
        var $flowPostHandler = $flowPostHandlerElement.find(".j-bpmn-flowposthandler");
        $flowPostHandler.tooltip({ title: J.msg['bpmn.flowPostHandlerTooltip'] })
        $flowPostHandler.rules('add', { json: true });
        $flowPostHandler.text(flowPostHandler);
        $flowPostHandler.blur(function() {
            var newFlowPostHandler = $flowPostHandler.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Post", newFlowPostHandler);
        });        
        
        // Listener
        var listeners = getExtensionBody(bo, "easyflow:Listeners");
        var listenersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.listeners'] + ':</label> <textarea class="form-control j-listeners" name="j-listeners"></textarea></div>'
            + '</div>';
        var $listenersElement = $(listenersHtml).appendTo($infoPannel);
        var $listeners = $listenersElement.find(".j-listeners");
        $listeners.tooltip({ title: J.msg['bpmn.listenersTooltip'] })
        $listeners.rules('add', { json: true });
        $listeners.text(listeners);
        $listeners.blur(function() {
            var newListeners = $listeners.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Listeners", newListeners);
        });
        // Filter
        var filters = getExtensionBody(bo, "easyflow:Filters");
        var filtersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.filters'] + ':</label> <textarea class="form-control j-filters" name="j-filters"></textarea></div>'
            + '</div>';
        var $filtersElement = $(filtersHtml).appendTo($infoPannel);
        var $filters = $filtersElement.find(".j-filters");
        $filters.tooltip({ title: J.msg['bpmn.filtersTooltip'] });
        $filters.rules('add', { json: true });
        $filters.text(filters);
        $filters.blur(function() {
            var newFilters = $filters.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Filters", newFilters);
        });
        // Node filter
        var nodeFilters = getExtensionBody(bo, "easyflow:NodeFilters");
        var nodeFiltersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.nodeFilters'] + ':</label> <textarea class="form-control j-nodefilters" name="j-nodefilters"></textarea></div>'
            + '</div>';
        var $nodeFiltersElement = $(nodeFiltersHtml).appendTo($infoPannel);
        var $nodeFilters = $nodeFiltersElement.find(".j-nodefilters");
        $nodeFilters.tooltip({ title: J.msg['bpmn.nodeFiltersTooltip'] });
        $nodeFilters.rules('add', { json: true });
        $nodeFilters.text(nodeFilters);
        $nodeFilters.blur(function() {
            var newNodeFilters = $nodeFilters.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:NodeFilters", newNodeFilters);
        });
        // Node pre handler filter
        var nodePreHandlerFilters = getExtensionBody(bo, "easyflow:NodePreHandlerFilters");
        var nodePreHandlerFiltersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.nodePreHandlerFilters'] + ':</label> <textarea class="form-control j-nodeprehandlerfilters" name="j-nodeprehandlerfilters"></textarea></div>'
            + '</div>';
        var $nodePreHandlerFiltersElement = $(nodePreHandlerFiltersHtml).appendTo($infoPannel);
        var $nodePreHandlerFilters = $nodePreHandlerFiltersElement.find(".j-nodeprehandlerfilters");
        $nodePreHandlerFilters.tooltip({ title: J.msg['bpmn.nodePreHandlerFiltersTooltip'] })
        $nodePreHandlerFilters.rules('add', { json: true });
        $nodePreHandlerFilters.text(nodePreHandlerFilters);
        $nodePreHandlerFilters.blur(function() {
            var newNodePreHandlerFilters = $nodePreHandlerFilters.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:NodePreHandlerFilters", newNodePreHandlerFilters);
        });        
        // Node action filter
        var nodeActionFilters = getExtensionBody(bo, "easyflow:NodeActionFilters");
        var nodeActionFiltersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.nodeActionFilters'] + ':</label> <textarea class="form-control j-nodeactionfilters" name="j-nodeactionfilters"></textarea></div>'
            + '</div>';
        var $nodeActionFiltersElement = $(nodeActionFiltersHtml).appendTo($infoPannel);
        var $nodeActionFilters = $nodeActionFiltersElement.find(".j-nodeactionfilters");
        $nodeActionFilters.tooltip({ title: J.msg['bpmn.nodeActionFiltersTooltip'] })
        $nodeActionFilters.rules('add', { json: true });
        $nodeActionFilters.text(nodeActionFilters);
        $nodeActionFilters.blur(function() {
            var newNodeActionFilters = $nodeActionFilters.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:NodeActionFilters", newNodeActionFilters);
        });
        // Node post handler filter
        var nodePostHandlerFilters = getExtensionBody(bo, "easyflow:NodePostHandlerFilters");
        var nodePostHandlerFiltersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.nodePostHandlerFilters'] + ':</label> <textarea class="form-control j-nodeposthandlerfilters" name="j-nodeposthandlerfilters"></textarea></div>'
            + '</div>';
        var $nodePostHandlerFiltersElement = $(nodePostHandlerFiltersHtml).appendTo($infoPannel);
        var $nodePostHandlerFilters = $nodePostHandlerFiltersElement.find(".j-nodeposthandlerfilters");
        $nodePostHandlerFilters.tooltip({ title: J.msg['bpmn.nodePostHandlerFiltersTooltip'] })
        $nodePostHandlerFilters.rules('add', { json: true });
        $nodePostHandlerFilters.text(nodePostHandlerFilters);
        $nodePostHandlerFilters.blur(function() {
            var newNodePostHandlerFilters = $nodePostHandlerFilters.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:NodePostHandlerFilters", newNodePostHandlerFilters);
        });        
        // Flow runner
        var runner = getExtensionBody(bo, "easyflow:Runner");
        var runnerHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.runner'] + ':</label> <textarea class="form-control j-bpmn-runner" name="j-bpmn-runner"></textarea></div>'
            + '</div>';
        var $runnerElement = $(runnerHtml).appendTo($infoPannel);
        var $runner = $runnerElement.find(".j-bpmn-runner");
        $runner.tooltip({ title: J.msg['bpmn.runnerTooltip'] })
        $runner.rules('add', { json: true });
        $runner.text(runner);
        $runner.blur(function() {
            var newRunner = $runner.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Runner", newRunner);
        });
        // Flow parse listeners 
        var parseListenersExp = getExtensionBody(bo, "easyflow:ParseListeners");
        var parseListenersHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.parseListeners'] + ':</label> <textarea class="form-control j-bpmn-parselisteners" name="j-bpmn-parselisteners"></textarea></div>'
            + '</div>';
        var $parseListenersElement = $(parseListenersHtml).appendTo($infoPannel);
        var $parseListeners = $parseListenersElement.find(".j-bpmn-parselisteners");
        $parseListeners.tooltip({ title: J.msg['bpmn.parseListenersTooltip'] })
        $parseListeners.rules('add', { json: true });
        $parseListeners.text(parseListenersExp);
        $parseListeners.blur(function() {
            var newParseListeners = $parseListeners.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:ParseListeners", newParseListeners);
        });  
        // Log flag
        var logFlag = getExtensionBody(bo, "easyflow:LogFlag");
        var logFlagHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.logFlag'] + ':</label> ' +
               '<select class="form-control j-bpmn-logflag">' +
               '<option value="">' + J.msg['bpmn.default'] + '</option>' +
               '<option value="true">' + J.msg['bpmn.true'] + '</option>' +
               '<option value="false">' + J.msg['bpmn.false'] + '</option>' +
            '</select>' +
            '</div>';
        var $logFlagElement = $(logFlagHtml).appendTo($infoPannel);
        $logFlagElement.tooltip({ title: J.msg['bpmn.logFlagTooltip'] });
        var $logFlag = $logFlagElement.find(".j-bpmn-logflag");
        $logFlag.val(logFlag);
        $logFlag.blur(function() {
            var newLogFlag = $logFlag.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:LogFlag", newLogFlag);
        });  
    }
    // Script task
    J.BpmnControl.prototype._elementPannelRender["bpmn:ScriptTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        var bo = element.businessObject;
        var _self = this;
        // Script format
        var scriptFormat = bo.scriptFormat;
        var scriptFormatHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.scriptFormat'] + ':</label> ' +
               '<select class="form-control j-bpmn-scriptformat">' +
               '<option value="exp">' + J.msg['bpmn.scriptFormat.exp'] + '</option>' +
               '<option value="createExp">' + J.msg['bpmn.scriptFormat.createExp'] + '</option>' +
            '</select>' +
            '</div>';
        var $scriptFormatElement = $(scriptFormatHtml).appendTo($infoPannel);
        $scriptFormatElement.tooltip({ title: J.msg['bpmn.scriptFormatTooltip'] });
        var $scriptFormat = $scriptFormatElement.find(".j-bpmn-scriptformat");
        $scriptFormat.val(scriptFormat);
        $scriptFormat.blur(function() {
            var newScriptFormat = $scriptFormat.val();
            if (newScriptFormat == 'exp') {
                delete bo.scriptFormat;
            } else {
                bo.scriptFormat = newScriptFormat;
            }
        });
        // task script
        var script = bo.script;
        var scriptHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.script'] + ':</label> <textarea class="form-control j-bpmn-script" name="j-bpmn-script"></textarea></div>'
            + '</div>';
        var $scriptElement = $(scriptHtml).appendTo($infoPannel);
        $scriptElement.tooltip({ title: J.msg['bpmn.scriptTooltip'] });
        var $script = $scriptElement.find(".j-bpmn-script");
        $script.text(script);
        $script.blur(function() {
            var newScript = $script.val();
            bo.script = newScript;
            _self._comment(element);
        });
        
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // User task
    J.BpmnControl.prototype._elementPannelRender["bpmn:UserTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Receive task
    J.BpmnControl.prototype._elementPannelRender["bpmn:ReceiveTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Task
    J.BpmnControl.prototype._elementPannelRender["bpmn:Task"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }      
    // Send task
    J.BpmnControl.prototype._elementPannelRender["bpmn:SendTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }    
    // Manual Task
    J.BpmnControl.prototype._elementPannelRender["bpmn:ManualTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }   
    // BusinessRuleTask Task
    J.BpmnControl.prototype._elementPannelRender["bpmn:BusinessRuleTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }   
    // Service Task
    J.BpmnControl.prototype._elementPannelRender["bpmn:ServiceTask"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }             
    // Call activity
    J.BpmnControl.prototype._elementPannelRender["bpmn:CallActivity"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        var bo = element.businessObject;
        var _self = this;
        // Called element
        var calledElement = bo.calledElement;
        var calledElementHtml = '<div class="row">' +
            '<div class="form-group col"><span class="j-require">*</span><label>' + J.msg['bpmn.calledElement'] + ':</label> ' +
               '<input class="form-control j-bpmn-calledelement"/>' +
            '</div>';
        var $calledElementElement = $(calledElementHtml).appendTo($infoPannel);
        $calledElementElement.tooltip({ title: J.msg['bpmn.calledElementTooltip'] });
        var $calledElement = $calledElementElement.find(".j-bpmn-calledelement");
        $calledElement.val(calledElement);
        $calledElement.blur(function() {
            var newCalledElement = $calledElement.val();
                bo.calledElement = newCalledElement;
        });
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Sub process
    J.BpmnControl.prototype._elementPannelRender["bpmn:SubProcess"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        var bo = element.businessObject;
        var _self = this;
        // Flow
        var flow = getExtensionBody(bo, "easyflow:Flow");
        var flowHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.flow'] + ':</label> <textarea class="form-control j-bpmn-flow" name="j-bpmn-flow"></textarea></div>'
            + '</div>';
        var $flowElement = $(flowHtml).appendTo($infoPannel);
        var $flow = $flowElement.find(".j-bpmn-flow");
        $flow.tooltip({ title: J.msg['bpmn.flowTooltip'] })
        $flow.rules('add', { json: true });
        $flow.text(flow);
        $flow.blur(function() {
            var newFlow = $flow.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Flow", newFlow);
        });
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }    
    // Transaction
    J.BpmnControl.prototype._elementPannelRender["bpmn:Transaction"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        var bo = element.businessObject;
        var _self = this;
        // Flow
        var flow = getExtensionBody(bo, "easyflow:Flow");
        var flowHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.flow'] + ':</label> <textarea class="form-control j-bpmn-flow" name="j-bpmn-flow"></textarea></div>'
            + '</div>';
        var $flowElement = $(flowHtml).appendTo($infoPannel);
        var $flow = $flowElement.find(".j-bpmn-flow");
        $flow.tooltip({ title: J.msg['bpmn.flowTooltip'] })
        $flow.rules('add', { json: true });
        $flow.text(flow);
        $flow.blur(function() {
            var newFlow = $flow.val();
            updateExtensionBody(_self.bpmnModeler, bo, "easyflow:Flow", newFlow);
        });
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeStart"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }        
    // Start event
    J.BpmnControl.prototype._elementPannelRender["bpmn:StartEvent"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // End event
    J.BpmnControl.prototype._elementPannelRender["bpmn:EndEvent"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
    }
    // Catch event
    J.BpmnControl.prototype._elementPannelRender["bpmn:IntermediateCatchEvent"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Throw event
    J.BpmnControl.prototype._elementPannelRender["bpmn:IntermediateThrowEvent"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:ExtConditionType"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }    
    // Exclusive gateway
    J.BpmnControl.prototype._elementPannelRender["bpmn:ExclusiveGateway"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Inclusive gateway
    J.BpmnControl.prototype._elementPannelRender["bpmn:InclusiveGateway"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Parallel gateway
    J.BpmnControl.prototype._elementPannelRender["bpmn:ParallelGateway"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }
    // Complex gateway
    J.BpmnControl.prototype._elementPannelRender["bpmn:ComplexGateway"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }  
    // Event based gateway
    J.BpmnControl.prototype._elementPannelRender["bpmn:EventBasedGateway"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePre"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodeAction"].call(this, $infoPannel, element);
        this._elementPannelRender["bpmn:NodePost"].call(this, $infoPannel, element);
    }       
    // Sequence flow
    J.BpmnControl.prototype._elementPannelRender["bpmn:SequenceFlow"] = function($infoPannel, element) {
        this._elementPannelRender["bpmn:Element"].call(this, $infoPannel, element, false);
        var bo = element.businessObject;
        var _self = this;
        // Conditional expression
        var conditionExpression = bo.conditionExpression ? bo.conditionExpression.body : '';
        var conditionExpressionHtml = '<div class="row">' +
            '<div class="form-group col"><label>' + J.msg['bpmn.conditionExp'] + ':</label> <textarea class="form-control j-bpmn-conditionExpression" name="j-bpmn-conditionExpression"></textarea></div>'
            + '</div>';
        var $conditionExpressionElement = $(conditionExpressionHtml).appendTo($infoPannel);
        $conditionExpressionElement.tooltip({ title: J.msg["bpmn.conditionExpTooltip"] });
        var $conditionExpression = $conditionExpressionElement.find(".j-bpmn-conditionExpression");
        $conditionExpression.text(conditionExpression);
        $conditionExpression.blur(function() {
            var newConditionExpression = $conditionExpression.val();
            if (!newConditionExpression) {
                delete bo.conditionExpression;
            } else {
                const moddle = _self.bpmnModeler.get('moddle');
                (!bo.conditionExpression) && (bo.conditionExpression = moddle.create('bpmn:FormalExpression'));
                bo.conditionExpression.body = newConditionExpression;
            }
            var eventBus = _self.bpmnModeler.get('eventBus');
            eventBus.fire("element.changed", { element: element });
            _self._comment(element);
        });
    }

    J.BpmnControl.prototype._commentAll = function() {
        var commentType = this.$bpmnContainer.find(".j-bpmn-comment-select").val();
        var _self = this;
        var elementRegistry = this.bpmnModeler.get('elementRegistry');
        var commentTypes = ["bpmn:StartEvent", "bpmn:EndEvent", "bpmn:IntermediateCatchEvent","bpmn:IntermediateThrowEvent",
            "bpmn:ExclusiveGateway", "bpmn:InclusiveGateway", "bpmn:ParallelGateway", "bpmn:ComplexGateway",, "bpmn:EventBasedGateway",
            "bpmn:ScriptTask", "bpmn:ReceiveTask", "bpmn:UserTask","bpmn:UserTask","bpmn:CallActivity","bpmn:SubProcess","bpmn:Transaction",
            "bpmn:Task","bpmn:SendTask","bpmn:ManualTask","bpmn:BusinessRuleTask","bpmn:ServiceTask",
            "bpmn:SequenceFlow"];
        var commentElements = elementRegistry.filter(function(e) {
            return commentTypes.indexOf(e.type) >= 0;
        });
        commentElements.forEach(function(element) {
            _self._comment(element, commentType);
        });
    }

    /**
     * Comment
     */
    J.BpmnControl.prototype._comment = function(element, commentType) {
        var commentTypes = ["bpmn:StartEvent", "bpmn:EndEvent", "bpmn:IntermediateCatchEvent","bpmn:IntermediateThrowEvent",
            "bpmn:ExclusiveGateway", "bpmn:InclusiveGateway", "bpmn:ParallelGateway", "bpmn:ComplexGateway",, "bpmn:EventBasedGateway",
            "bpmn:ScriptTask", "bpmn:ReceiveTask", "bpmn:UserTask","bpmn:CallActivity","bpmn:SubProcess","bpmn:Transaction",
           "bpmn:Task", "bpmn:SendTask","bpmn:ManualTask","bpmn:BusinessRuleTask","bpmn:ServiceTask",
            "bpmn:SequenceFlow"];
            if (! commentTypes.includes(element.type)) {
                return;
            }
        if (commentType === undefined) {
            commentType = this.$bpmnContainer.find(".j-bpmn-comment-select").val();
        }
        var _self = this;
        var overlays = this.bpmnModeler.get('overlays');
        var bo = element.businessObject;
        overlays.remove({ element: element.id, type:"easyflow-comment"});
        if (!commentType.includes("node") && !commentType.includes("condition")) {
            return;
        }
        // Judge type
        if ("bpmn:SequenceFlow" == element.type) {
            if (!commentType.includes("condition") || !bo.conditionExpression) {
                return;
            }
        }
        if (["bpmn:StartEvent", "bpmn:EndEvent", "bpmn:IntermediateCatchEvent","bpmn:IntermediateThrowEvent",
            "bpmn:ExclusiveGateway", "bpmn:InclusiveGateway", "bpmn:ParallelGateway", "bpmn:ComplexGateway",, "bpmn:EventBasedGateway",
            "bpmn:ScriptTask", "bpmn:ReceiveTask", "bpmn:UserTask","bpmn:CallActivity","bpmn:SubProcess","bpmn:Transaction",
            "bpmn:Task","bpmn:SendTask","bpmn:ManualTask","bpmn:BusinessRuleTask","bpmn:ServiceTask",
            "bpmn:SubProcess","bpmn:Transaction"].includes(element.type)) {
            if (!commentType.includes("node")) {
                return;
            }
        }
        
        var elementRegistry = this.bpmnModeler.get('elementRegistry');
        var element = elementRegistry.get(element.id);
        if (! element) {
            return;
        }
        
        var commentId = "comment_" + $.jSequence.next();
        var clazz = "";
        if("bpmn:SequenceFlow" == element.type) {
            clazz = " j-bpmn-comment-condition";
        }
        var html = '<div class="j-bpmn-comment' + clazz + '" id="' + commentId + '"><dl>';
            html += '<dt>ID:</dt><dd>' + bo.id + (bo.name?"(" + bo.name + ")":"") + '</dd>';
        if (element.type == "bpmn:SequenceFlow") {
            html += '<dt>' + J.msg['bpmn.condition'] + ':</dt><dd>' + (bo.conditionExpression ? bo.conditionExpression.body : J.msg['bpmn.none']) + '</dd>'
        }
        if (element.type == 'bpmn:ScriptTask') {
            html += '<dt>' + J.msg['bpmn.script'] + ':</dt><dd>' + (bo.script ? bo.script : J.msg['bpmn.none']) + '</dd>';
        }
        if (element.type=='bpmn:CallActivity') {
            html += '<dt>' + J.msg['bpmn.calledElement'] + ':</dt><dd>' + (bo.calledElement ? bo.calledElement : J.msg['bpmn.none']) + '</dd>';
        }
            var properties = getExtensionBody(bo, "easyflow:Properties");
            if (properties) {
                html += '<dt>' + J.msg['bpmn.property'] + ':</dt><dd>' + properties + '</dd>';
            }
        if (element.type == 'bpmn:ScriptTask' || element.type=='bpmn:UserTask' || element.type=='bpmn:ReceiveTask') {
           var conditionType = getExtensionBody(bo, "easyflow:ConditionType");
            if (conditionType == 'exclusive') {
                html += '<dt>' + J.msg['bpmn.branchType'] + ':</dt><dd>' + J.msg['bpmn.branchType.exclusive'] + '</dd>';
            }
            var start = getExtensionBody(bo, "easyflow:Start");
            if (start == 'true') {
                html += '<dt>' + J.msg['bpmn.startNode'] + ':</dt><dd>' + J.msg['bpmn.startNode.true'] + '</dd>';
            }            
        }
        
        var nodePre = getExtensionBody(bo, "easyflow:Pre");
        if (nodePre) {
            html += '<dt>' + J.msg['bpmn.selfPre'] + ':</dt><dd>' + nodePre + '</dd>';
        }
         var nodeAction = getExtensionBody(bo, "easyflow:Action");
        if (nodeAction) {
            html += '<dt>' + J.msg['bpmn.selfAction'] + ':</dt><dd>' + nodeAction + '</dd>';
        }    
         var nodePost = getExtensionBody(bo, "easyflow:Post");
        if (nodePost) {
            html += '<dt>' + J.msg['bpmn.selfPost'] + ':</dt><dd>' + nodePost + '</dd>';
        }               
 
        
        html += '</dl></div>';
        // attach an overlay to a node
        var extPropertiesStr = getExtensionBody(bo, "easyflow:ExtProperties");
        var extProperties = extPropertiesStr ? JSON.parse(extPropertiesStr) : {};
        var pos = extProperties && extProperties.commentPosition ? extProperties.commentPosition : { top:3, left:20 };
        overlays.add(element.id, "easyflow-comment", {
            position: pos,
            html: html,
            scale:{max:1}
        });
        var $comment = $("#" + commentId);
        var $commentParent = $comment.parent();
        var width = extProperties && extProperties.commentWidth ? extProperties.commentWidth : 110;
        $comment.width(width);
        // Drag event
        $comment.on("mousemove", function(e){
            var offset = $comment.offset();
            if ((offset.left + $comment.width() - e.pageX) < 5) {
                $comment.css("cursor", "e-resize");
            } else if (e.pageX-offset.left < 5 || e.pageY-offset.top<5 || offset.top+$comment.height() - e.pageY < 5) {
                $comment.css("cursor", "move");
            } else {
                $comment.css("cursor", "auto");
            }  
        });
        $comment.on("mousedown", function(e) {
            $comment.pageX = e.pageX;
            $comment.pageY = e.pageY;
            var offset = $comment.offset();
            if ((offset.left + $comment.width() - e.pageX) < 5) {
                $comment.eventType = 'width';
                $comment.css("cursor", "e-resize");
            } else if (e.pageX-offset.left < 5 || e.pageY-offset.top<5 || offset.top+$comment.height() - e.pageY < 5) {
                $comment.eventType = "drag";
                $comment.css("cursor", "move");
            } else {
                $comment.eventType = null;
                $comment.css("cursor", "auto");
            }
            var mousemoveFunction = function(e) {
                if ($comment.eventType == "drag") {
                    var top = pos.top + (e.pageY - $comment.pageY);
                    var left = pos.left + (e.pageX - $comment.pageX);
                    $commentParent.css({ top: top + "px", left: left + "px" });
                } else if ($comment.eventType == 'width') {
                    var currentWidth = width + (e.pageX - $comment.pageX);
                    $comment.width(currentWidth);
                }
            };
            var mouseupFunction = function(e) {
                $(document).off("mousemove", mousemoveFunction).off("mouseup", mouseupFunction);
                var extPropertiesStr = getExtensionBody(bo, "easyflow:ExtProperties");
                var extProperties = extPropertiesStr ? JSON.parse(extPropertiesStr) : {};
                $comment.css("cursor", "auto");
                if ($comment.eventType == "drag") {
                    pos.left += e.pageX - $comment.pageX;
                    pos.top += e.pageY - $comment.pageY;                   
                    extProperties.commentPosition = pos;
                    updateExtensionBody(_self.bpmnModeler, bo, "easyflow:ExtProperties", JSON.stringify(extProperties));
                } else if ($comment.eventType=='width') {
                    width += e.pageX - $comment.pageX;
                    extProperties.commentWidth = width;
                    updateExtensionBody(_self.bpmnModeler, bo, "easyflow:ExtProperties", JSON.stringify(extProperties));
                }
            }
            if ($comment.eventType){
                $(document).on("mousemove", mousemoveFunction).on("mouseup", mouseupFunction)
            }
        });
    }
    /**
     * html of show
     */
    J.BpmnControl.prototype._html = function() {
        return '<div class="j-bpmn-pannel j-bpmn-pannel-normal">' +                                                           
          '<div class="j-flow-canvas"></div> ' +    
          '<div class="text-left j-bpmn-btn-group">     ' +                                                                   
              '<button type="button" class="btn btn-primary j-bpmn-view">' + J.msg['bpmn.bpmnDefinition'] + '</button>\n' +
            (this.cfg.mode == 'view' ? '' : '<button type="button" class="btn btn-secondary j-bpmn-import">' + J.msg['bpmn.import'] + '</button>\n') +
              '<button type="button" class="btn btn-secondary j-bpmn-export">' + J.msg['bpmn.export'] + '</button>\n' +
              '<button type="button" class="btn btn-secondary j-bpmn-svgdownload">' + J.msg['bpmn.svg'] + '</button>\n' +
              '<button type="button" class="btn btn-secondary j-bpmn-zoomin" data-toggle="tooltip" title="' + J.msg['bpmn.zoomIn'] + '">-</button>\n' +
              '<button type="button" class="btn btn-secondary j-bpmn-zoomout" data-toggle="tooltip" title="' + J.msg['bpmn.zoomOut'] + '">+</button>\n' +
              '<button type="button" class="btn btn-secondary j-bpmn-fullscreen" data-toggle="tooltip" title="' + J.msg['bpmn.fullScreen'] + '"><i class="fas fa-expand"></i></button>\n' +
              '<div class="j-bpmn-comment-select-container"><select multiple="multiple" class="j-bpmn-comment-select d-none">' +
                  '<option value="node">' + J.msg['bpmn.node'] + '</option>' +
                  '<option value="condition">' + J.msg['bpmn.condition'] + '</option>' +
              '</select></div> ' +
          '</div>  ' +                                                
         '<div class="infoPannel"><div class="infoPannelHead"><span class="j-bpmn-info-title"><b>' + J.msg['bpmn.infoPannel'] + '</b></span><i class="fa fa-info-circle j-bpmn-info-switch"></i><div style="clear:both"></div></div><form class="container"></form></div>  ' +                                          
      '</div>    ';
    }

    J.BpmnControl.prototype._bpmnViewHtml = function() {
        return '<div class="bpmnDefContainer">                                                                                 ' +
            '    <div class="row">                                                                                          ' +
            '        <div class="form-group col">                                                                           ' +
            '            <label><span class="j-require">*</span>' + J.msg['bpmn.flowBpmnDefinition'] + ':</label>                                       ' +
            '            <textarea class="bpmnDef form-control" rows="30"></textarea>                                       ' +
            '        </div>                                                                                                 ' +
            '    </div>                                                                                                     ' +
            '    <div class="row">                                                                                          ' +
            '        <div class="col text-center">                                                                          ' +
            (this.cfg.mode == 'view' ? '' : '            <button type="button" class="btn btn-primary j-btn-bpmn-render">' + J.msg['bpmn.renderFlowDiagram'] +'</button>              ') +
            '            <button type="button" class="btn btn-secondary j-btn-primary j-btn-bpmn-json">' + J.msg['bpmn.viewFlowDefinition'] + '</button>           ' +
            (this.cfg.mode != 'edit' ? '' : '            <button type="button" class="btn btn-secondary j-btn-compare-bpmn">' + J.msg['bpmn.oldNewBpmnCompare'] + '</button>       ') +
            (this.cfg.mode != 'edit' ? '' : '            <button type="button" class="btn btn-secondary j-btn-compare-easyflow">' + J.msg['bpmn.oldNewEasyFlowCompare'] + '</button>   ') +
            '            <button type="button" class="btn btn-secondary j-btn-cancel">' + J.msg['bpmn.cancel'] + '</button>                         ' +
            '        </div>                                                                                                 ' +
            '    </div>                                                                                                     ' +
            '</div>                                                                                                         ' +
            '';
    }

    J.BpmnControl.prototype.initialDiagram = '<?xml version="1.0" encoding="UTF-8"?>'
        + '<bpmn:definitions xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
        'xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL" ' +
        'xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI" ' +
        'xmlns:dc="http://www.omg.org/spec/DD/20100524/DC" ' +
        'targetNamespace="http://bpmn.io/schema/bpmn" ' +
        'id="Definitions_1">'
        + '<bpmn:process id="Process_1" isExecutable="false">'
        + '<bpmn:startEvent id="StartEvent_1"/>'
        + '</bpmn:process>'
        + '<bpmndi:BPMNDiagram id="BPMNDiagram_1">'
        + '<bpmndi:BPMNPlane id="BPMNPlane_1" bpmnElement="Process_1">'
        + '<bpmndi:BPMNShape id="_BPMNShape_StartEvent_2" bpmnElement="StartEvent_1">'
        + '<dc:Bounds height="36.0" width="36.0" x="173.0" y="102.0"/>'
        + '</bpmndi:BPMNShape>' + '</bpmndi:BPMNPlane>'
        + '</bpmndi:BPMNDiagram>' + '</bpmn:definitions>';

/**
* Find BPMN Element
*/
    function _findProcessElement(bpmnModeler) {
        var elementRegistry = bpmnModeler.get('elementRegistry');
        var processes = elementRegistry.filter(function(element) {
            return element.type == 'bpmn:Process';
        });
        if (processes.length>0) {
            return processes;
        }
        var participants = elementRegistry.filter(function(element) {
            return element.type == 'bpmn:Participant';
        });
        participants.forEach(function(element){
            var processBusinessObject = element.businessObject.processRef;
            if (processBusinessObject) {
              processes.push({businessObject:processBusinessObject});
            }
        });
        return processes;
    }
    

    /**
     * Read extension field
     */
    function getExtensionElement(element, type) {
        if (!element.extensionElements || !element.extensionElements.values) {
            return;
        }
        return element.extensionElements.values.filter((extensionElement) => {
            return extensionElement.$instanceOf(type);
        })[0];
    }

    function getExtensionBody(bo, type) {
        var extensionElement = getExtensionElement(bo, type);
        return extensionElement ? extensionElement.$body : null;
    }

    /**
     * Update body of extension
     */
    function updateExtensionBody(bpmnModeler, bo, type, value) {
        // Delete element
        if (!value) {
            if (!bo.extensionElements || !bo.extensionElements.values) {
                return;
            }
            var values = [];
            bo.extensionElements.values && bo.extensionElements.values.forEach(function(el) {
                if (!el.$instanceOf(type)) {
                    values.push(el);
                }
            });
            if (values.length == 0) {
                delete bo.extensionElements;
            } else {
                bo.extensionElements.values = values;
            }
        } else {
            // Add or modify element
            var element = getExtensionElement(bo, type);
            if (element) {
                // modify element
                element.$body = value;
            } else {
                // add element
                const moddle = bpmnModeler.get('moddle'),
                    modeling = bpmnModeler.get('modeling');
                (!bo.extensionElements) && (bo.extensionElements = moddle.create('bpmn:ExtensionElements'));
                var element = moddle.create(type);
                element.$body = value;
                bo.extensionElements.get('values').push(element);
            }
        }
    }

    var easyflowExtension =
    {
        "name": "easyflow",
        "uri": "http://jd.com/easyflow",
        "prefix": "easyflow",
        "xml": {
            "tagAlias": "lowerCase"
        },
        "types": [
            {
                "name": "Properties",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "Listeners",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "Filters",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "NodeFilters",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "NodePreHandlerFilters",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "NodeActionFilters",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "NodePostHandlerFilters",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },                        
            {
                "name": "ConditionType",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "Start",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },   
            {
                "name": "Pre",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            }, 
            {
                "name": "Action",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },                         
            {
                "name": "Post",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },   			
            {
                "name": "Runner",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "ParseListeners",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },                    
            {
                "name": "ExtProperties",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "Flow",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            },
            {
                "name": "LogFlag",
                "superClass": ["Element"],
                "properties": [
                    {
                        "name": "$body",
                        "isBody": true,
                        "type": "String"
                    }
                ]
            }                                
        ]
    };



    function easyFlowTranslate(template, replacements) {
        replacements = replacements || {};

        // Translate
        template = J.bpmnIOTranslations[template] || template;

        // Replace
        return template.replace(/{([^}]+)}/g, function(_, key) {
            return replacements[key] || '{' + key + '}';
        });
    }


})(window.J);                