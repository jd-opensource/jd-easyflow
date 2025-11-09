(function (J) {

	J.PageConfig = J.Components['pageConfig'] = function(cfg) {
		this.componentData = cfg.data &&cfg.data.component;
		J.BaseComponent.call(this, 'pageConfig', cfg);
		this.render = function($container){
           this.$container = $container;
			console.log("Render pageConfig");
			var formHtml = '<form class="j-form-horizontal j-page-config-form ' + this.componentClass + '" action="javascript:void(0)"></form>';
			this.$form = $(formHtml).appendTo($container);
			this.$form.validate();
			this.component.render(this.$form);
            
            var formConfigHtml = '';
			
			var pageConfigHtml0 = 
				'<div class="row mt-1">' +
				    '<div class="form-group col-2">' +
				        '<label>NULL' + J.msg["dataviewcfg.collectPolicy"] + ':</label><select class="form-control nullPolicy ' + this.componentClass + '"><option value="KEEP">'+J.msg['dataviewcfg.keep']+'</option><option value="IGNORE">'+J.msg['dataviewcfg.ignore']+'</option></select>' +
				    '</div>' +
				 '</div>' +
			 '</div>';
			
			var pageConfigHtml = 	
                '<div class="row mt-1">' +
                    '<div class="form-group col">' +
                        '<label>'+J.msg['dataviewcfg.topHtml']+':</label> <textarea class="form-control topHtml ' + this.componentClass + '"></textarea>' +
                    '</div>' +                
                    '<div class="form-group col">' +
                        '<label>' + J.msg['dataviewcfg.bottomHtml']+':</label> <textarea class="form-control bottomHtml ' + this.componentClass + '"></textarea>' +
                    '</div>' +
                '</div>' +              			
				'<div class="row mt-1">' +
				    '<div class="form-group col">' +
				        '<label>'+J.msg['dataviewcfg.pageBeforeRenderScript'] +':</label> <textarea class="form-control beforeRender ' + this.componentClass + '"></textarea>' +
				    '</div>' +
				    '<div class="form-group col">' +
			            '<label>'+J.msg['dataviewcfg.pageAfterRenderScript'] +':</label> <textarea class="form-control afterRender ' + this.componentClass + '"></textarea>' +
			        '</div>' +
			    '</div>' +	    
				'<div class="row mt-1">' +
				    '<div class="form-group col">' +
			            '<label>'+J.msg['dataviewcfg.pageBeforeCollectScript']+':</label> <textarea class="form-control beforeCollect ' + this.componentClass + '"></textarea>' +
			        '</div>' +
			        '<div class="form-group col">' +
		                '<label>'+J.msg['dataviewcfg.pageAfterCollectScript']+':</label> <textarea class="form-control afterCollect ' + this.componentClass + '"></textarea>' +
		            '</div>' +			        
			     '</div>';
			    this.$pageConfig0 = $(pageConfigHtml0).appendTo(this.$form);
				this.$pageConfig = $(pageConfigHtml).appendTo(this.$form);
				this.$nullPolicy = this.$pageConfig0.find(".nullPolicy." + this.componentClass);
				this.$nullPolicy.tooltip({title:J.msg['dataviewcfg.nullPolicyTooltip']})
				cfg.data && this.$nullPolicy.val(cfg.data.nullPolicy);

                this.$topHtml = this.$pageConfig.find(".topHtml." + this.componentClass);
                cfg.data && this.$topHtml.val(cfg.data.topHtml);                
                this.$bottomHtml = this.$pageConfig.find(".bottomHtml." + this.componentClass);
                cfg.data && this.$bottomHtml.val(cfg.data.bottomHtml);
                
				this.$beforeRender = this.$pageConfig.find(".beforeRender." + this.componentClass);
				cfg.data && this.$beforeRender.val(cfg.data.beforeRender);
				this.$afterRender = this.$pageConfig.find(".afterRender." + this.componentClass);
				cfg.data && this.$afterRender.val(cfg.data.afterRender);
				this.$beforeCollect = this.$pageConfig.find(".beforeCollect." + this.componentClass);
				cfg.data && this.$beforeCollect.val(cfg.data.beforeCollect);
				this.$afterCollect = this.$pageConfig.find(".afterCollect." + this.componentClass);
				cfg.data && this.$afterCollect.val(cfg.data.afterCollect);
              
                
                
		}
		this.collect = function(data) {
			data.type = "page";
			data.component = this.component.collect();
			data.nullPolicy = this.$nullPolicy.val();
            data.topHtml = this.$topHtml.val();
            data.bottomHtml = this.$bottomHtml.val();
			data.beforeRender = this.$beforeRender.val();
			data.afterRender = this.$afterRender.val();
			data.beforeCollect = this.$beforeCollect.val();
			data.afterCollect = this.$afterCollect.val();
			return data;
		}

	}
	J.TabsConfig = J.Components['tabsConfig'] = function(cfg) {
		this.componentData = cfg.data &&cfg.data.components;
		J.BaseComponent.call(this, 'tabsConfig', cfg);
		this.render = function($container){
            this.$container = $container;
			console.log("Render tabsConfig");
			this.component.render($container);
		}
		this.collect = function(data) {
			var tabsData = {};
			tabsData.type="tabs";
			tabsData.components = this.component.collect();
			return tabsData;
		}

	}

	J.TabConfig = J.Components['tabConfig'] = function(cfg) {
		this.componentData = cfg.data && cfg.data.components;
		J.BaseComponent.call(this, 'tabConfig', cfg);
		this.render = function($container) {
            this.$container = $container;
			console.log("Render tabConfig");
			var tabConfigHtml = 
			'<div class="j-tab-config card">' +
		         '<div class="card-body">' +
			        '<div class="row">' +
			            '<div class="form-group col-8">' +
			                '<label>'+J.msg['dataviewcfg.tabName']+':</label> <input type="text" class="form-control tabName ' + this.componentClass + '" value="" />' +
			            '</div>' +
                        '<div class="form-check col">' +
                            '<input type="checkbox" class="form-check-input tabSider ' + this.componentClass + '"/>' +
                            '<label class="form-check-label">'+J.msg['dataviewcfg.siderBar']+'</label>' + 
                        '</div>' +   
                        '<div class="form-group col-2"><label>'+J.msg['dataviewcfg.siderBarLevel']+':</label>' +
                            '<select class="form-control tabSiderLevel ' + this.componentClass + '"><option value="1">1</option><option value="2">2</option></select>' +
                        '</div>' +                                                 
                        '<div class="form-check col">' +
                            '<input type="checkbox" class="form-check-input tabShow ' + this.componentClass + '"/>' +
                            '<label class="form-check-label">'+J.msg['dataviewcfg.show']+'</label>' + 
                        '</div>' +                          
			        '</div>' +
		        '</div>' +
	        '</div>';
			this.$tabConfig = $(tabConfigHtml).appendTo($container);
			cfg.data && this.$tabConfig.find(".tabName." + this.componentClass).val(cfg.data.name);
            this.$sider = this.$tabConfig.find(".tabSider." + this.componentClass);
            this.data && this.$sider.prop("checked", this.data.sider);
            this.$siderLevel = this.$tabConfig.find(".tabSiderLevel." + this.componentClass);
            cfg.data && this.$siderLevel.val(cfg.data.siderLevel);
            this.$show = this.$tabConfig.find(".tabShow." + this.componentClass);
            this.$show.prop("checked", !(this.data && this.data.show===false));
			var $tabComponentContainer = this.$tabConfig.find(".card-body");
			this.component.render($tabComponentContainer);
		}
		this.collect = function(data) {
			var tabData = {};
			tabData.type = "tab";
			tabData.name = this.$tabConfig.find("." + this.componentClass).val();
            (! this.$show.prop("checked")) && (tabData.show=false);
            tabData.sider = this.$sider.prop("checked");
            (this.$siderLevel.val()==2) && (tabData.siderLevel = 2);
			tabData.components = this.component.collect();
			return tabData;
		}	
	}

	J.PanelConfig = J.Components['panelConfig'] = function(cfg) {
		this.componentData = cfg.data && cfg.data.components;
		J.BaseComponent.call(this, 'panelConfig', cfg);
		this.render = function($container) {
            this.$container = $container;
			console.log("Render panelConfig");
			var panelConfigHtml = 
			'<div class="j-panel-config card">' +
		         '<div class="card-body">' +
			        '<div class="row">' +
			            '<div class="form-group col-11">' +
			                '<label><span class="j-require"></span>'+J.msg['dataviewcfg.panelName']+':</label> <input type="text" class="form-control panelName ' + this.componentClass + '" value="" />' +
			            '</div>' +
                    '<div class="form-check col">' +
                    '<input type="checkbox" class="form-check-input panelShow ' + this.componentClass + '"/>' +
                    '<label class="form-check-label">'+J.msg['dataviewcfg.show']+'</label>' + 
                '</div>' +                           
			        '</div>' +
		        '</div>' +
	        '</div>';
			this.$panelConfig = $(panelConfigHtml).appendTo($container);
			cfg.data && this.$panelConfig.find(".panelName." + this.componentClass).val(cfg.data.name);
            this.$show = this.$panelConfig.find(".panelShow." + this.componentClass);
            this.$show.prop("checked", !(this.data && this.data.show===false));
			var $panelComponentContainer = this.$panelConfig.find(".card-body");
			this.component.render($panelComponentContainer);
		}
		this.collect = function(data) {
			var panelData = {};
			panelData.type = "panel";
			panelData.name = this.$panelConfig.find("." + this.componentClass).val();
            (! this.$show.prop("checked")) && (panelData.show=false);
			panelData.components = this.component.collect();
			return panelData;
		}
	}


	J.CardConfig = J.Components['cardConfig'] = function(cfg) {
		this.componentData = cfg.data && cfg.data.components;
		J.BaseComponent.call(this, 'cardConfig', cfg);
		this.render = function($container) {
            this.$container = $container;
			console.log("Render cardConfig");
			var cardConfigHtml = 
			'<div class="j-card-config card">' +
		         '<div class="card-body">' +
			        '<div class="row">' +
			            '<div class="form-group col-6">' +
			                '<label>'+J.msg['dataviewcfg.cardName']+':</label> <input type="text" class="form-control cardName ' + this.componentClass + '" value="" />' +
			            '</div>' +
			            '<div class="form-group col-4">' +
		                '<label>'+J.msg['dataviewcfg.elementPerRow']+':</label> <input type="text" class="form-control cardCols ' + this.componentClass + '" value="" ></input>' +
		            '</div>' +	
                    '<div class="form-check col">' +
                    '<input type="checkbox" class="form-check-input cardShow ' + this.componentClass + '"/>' +
                    '<label class="form-check-label">'+J.msg['dataviewcfg.show']+'</label>' + 
                '</div>' + 	            
			        '</div>' +
		        '</div>' +
	        '</div>';
			this.$cardConfig = $(cardConfigHtml).appendTo($container);
			cfg.data && this.$cardConfig.find(".cardName." + this.componentClass).val(cfg.data.name);
			cfg.data && this.$cardConfig.find(".cardCols." + this.componentClass).val(cfg.data.cols);
            this.$show = this.$cardConfig.find(".cardShow." + this.componentClass);
            this.$show.prop("checked", !(this.data && this.data.show===false));
			var $cardComponentContainer = this.$cardConfig.find(".card-body");
			this.component.render($cardComponentContainer);
		}
		this.collect = function(data) {
			var cardData = {};
			cardData.type = "card";
			cardData.name = this.$cardConfig.find(".cardName." + this.componentClass).val();
			cardData.cols = parseInt(this.$cardConfig.find(".cardCols." + this.componentClass).val());
            (! this.$show.prop("checked")) && (cardData.show=false);
			cardData.components = this.component.collect();
			return cardData;
		}
	}

	J.ElementConfig = J.Components['elementConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'elementConfig', cfg);
		this.render = function($container) {
            this.$container = $container;
			console.log("Render elementConfig");
			var configTypeList = J.DataView.configTypeList;
			if (! configTypeList) {
			  configTypeList = [
				{value: "inputText", "text":J.msg['dataviewcfg.textType']},
				{value: "select", "text":J.msg['dataviewcfg.selectType']},
				{value: "textarea", "text":J.msg['dataviewcfg.areaType']},
				{value: "fixInput", "text":J.msg['dataviewcfg.fixTextType']},
				{value: "tableSelect", "text":J.msg['dataviewcfg.tableSelectType']},
				{value: "listSelect", "text":J.msg['dataviewcfg.listSelectType']},
				{value: "self", "text":J.msg['dataviewcfg.selfType']},
				{value: "cardList", "text":J.msg['dataviewcfg.cardListType']},
				{value: "elementsCard", "text":J.msg['dataviewcfg.elementsCardType']},				
				{value: "table", "text":J.msg['dataviewcfg.tableType']},
			  ];
			}
			if (J.DataView.additionalConfigTypeList) {
				configTypeList=configTypeList.concat(J.DataView.additionalConfigTypeList);
			}			
			var configTypeListStr = "";
			for (var i in configTypeList) {
				configTypeListStr +='<option value="' + configTypeList[i].value + '">'+configTypeList[i].text + '</option>';
			}
			
			var elementConfigHtml = 
	        '<div class="row j-element-config">' +
		        '<div class="form-group col">' +
		            '<label>'+J.msg['dataviewcfg.configName']+':</label> <input type="text" class="form-control j-el-name" value="" />' +
		        '</div>' +
		        '<div class="form-group col">' +
		            '<label>'+J.msg['dataviewcfg.configType']+':</label>' +
	                '<select class="form-control j-el-type">' +
	                    '<option value="" selected="selected">'+J.msg['dataviewcfg.selectTypeTip']+'</option>' +
		                configTypeListStr+
	                '</select>' +
		        '</div>' +
		        '<div class="form-group col"><label>'+J.msg['dataviewcfg.gridColNum']+':</label>' +
				    '<select class="form-control c_cols ' + this.componentClass + '">' +
				        '<option value="-1" selected="selected">'+J.msg['dataviewcfg.gridColAuto']+'</option>' +
				        '<option value="1">1</option>' +
				        '<option value="2">2</option>' +
				        '<option value="3">3</option>' +
				        '<option value="4">4</option>' +
				        '<option value="5">5</option>' +
				        '<option value="6">6</option>' +
				    '</select></div>'+
				'<div class="form-group col"><label>'+J.msg['dataviewcfg.newLine']+':</label>' +
				    '<select class="form-control c_newline ' + this.componentClass + '">' +
		    	        '<option value="true">'+J.msg['dataviewcfg.yes']+'</option>' +
		                '<option value="false" selected="selected">'+J.msg['dataviewcfg.no']+'</option>' +
		            '</select>' +
			    '</div>' +
                '<div class="form-check col">' +
                    '<input type="checkbox" class="form-check-input c_show ' + this.componentClass + '"/>' +
                    '<label class="form-check-label">'+J.msg['dataviewcfg.show']+'</label>' + 
                '</div>' +                
		        '<div class="col j-el-info">' +
	            '</div>' +
	        '</div>';
			this.$elementConfig = $(elementConfigHtml).appendTo($container);
			this.$elementName = this.$elementConfig.find(".j-el-name");
			this.$elementInfo = this.$elementConfig.find(".j-el-info");
			this.$elementExt = null;// Extend config, currently list element using. add one row after j-element-config
			this.$elementType = this.$elementConfig.find(".j-el-type");
			this.$elementName.val(cfg.data && cfg.data.name);
			var _self = this;
			this.$elementType.change(function(){
				_self.$elementInfo.empty();
				_self.$elementExt && _self.$elementExt.remove();
				var val = $(this).val();
				if (val == "") {
					return;
				}
				_self.component = new J.Components[val + "Config"](_self.cfg);
				_self.component.render(_self.$elementInfo);
			});
			cfg.data && cfg.data.type && this.$elementType.val(cfg.data.type).change();
			
			this.$cols = this.$elementConfig.find(".c_cols");
			this.$cols.tooltip({title:J.msg['dataviewcfg.colsTooltip']})
			this.data && this.data.cols && this.$cols.val(this.data.cols);
			this.$newline = this.$elementConfig.find(".c_newline");
			this.$newline.tooltip({title:J.msg['dataviewcfg.newLineTooltip']});
			this.data && this.$newline.val(this.data.newline?"true":"false");
            this.$show = this.$elementConfig.find(".c_show");
            var show = !(this.data && this.data.show===false);
            this.$show.prop("checked", show);
		}
		
		this.collect = function(data) {
			var elementType = this.$elementType.val();
			if (! elementType) {return;}
			var elementName = this.$elementName.val();
			var cols = parseInt(this.$cols.val());
			var newline = this.$newline.val()=='true';
			var elementInfo = this.component.collect();
			if (!elementInfo.type) {
				elementInfo.type = elementType;
			}
			elementInfo.name = elementName;
			elementInfo.cols = cols;
			elementInfo.newline = newline;
            (! this.$show.prop("checked")) && (elementInfo.show=false);
			return elementInfo;
		}
		
	}
	var _commonAddConfigBody = J._commonAddConfigBody = function($container) {
		var bodyHtml = '<div><button type="button" class="btn btn-primary j-param-config">'+J.msg['dataviewcfg.paramConfig']+'</button></div>' +
			'<div class="modal modal" tabindex="-1" role="dialog">'+
			  '<div class="modal-dialog  modal-xl" role="document">'+
			    '<div class="modal-content">'+
			      '<div class="modal-header">'+
			        '<h5 class="modal-title">'+J.msg['dataviewcfg.config']+'</h5>'+
			        '<button type="button" class="close" data-dismiss="modal" aria-label="Close">'+
			          '<span aria-hidden="true">&times;</span>'+
			        '</button>'+
			      '</div>'+
			      '<div class="modal-body"></div>'+
			      '<div class="modal-footer">'+
			        '<button type="button" class="btn btn-primary" data-dismiss="modal">'+J.msg['dataviewcfg.confirm']+'</button>'+
			      '</div>'+
			    '</div>'+
			  '</div>'+
			'</div>';
		$(bodyHtml).appendTo($container);
		var _self = this;
		this.$bodyContainer = $container.find(".modal-body");
		this.$modal = $container.find(".modal");
		this.$paramConfigBtn = $container.find(".j-param-config");
		this.$paramConfigBtn.click(function(){
			_self.$modal.modal();
		});
	}

	var _commonConfigRender = J._commonConfigRender = function($container) {
		var _self = this;
		var $row = $("<div class='row mt-2'></div>").appendTo($container);
		var requireHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.required']+':</label>' +
			'<select class="form-control c_require ' + this.componentClass + '">' +
	    	'<option value="true">'+J.msg['dataviewcfg.yes']+'</option>' +
	        '<option value="false" selected="selected">'+J.msg['dataviewcfg.no']+'</option>' +
	    '</select></div>';
		var $require = $(requireHtml).appendTo($row);
		this.data && $require.find("." + this.componentClass).val(this.data.required===false ? "false":"true");
		var descHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.configDesc']+':</label>' +
			'<textarea class="form-control j-textarea-config c_desc ' + this.componentClass + '"></textarea></div>';	
		var $desc = $(descHtml).appendTo($row);
		$desc.tooltip({title:J.msg['dataviewcfg.descTooltip']});
		this.data && this.data.desc && $desc.find("." + this.componentClass).val(this.data.desc);
		
		$row = $("<div class='row'></div>").appendTo($container);
		// Modify flag
		var modify0Html = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.modifyOnAdd']+':</label>' +
			'<select class="form-control c_modify0 ' + this.componentClass + '">' +
	    	'<option value="true" selected="selected">'+J.msg['dataviewcfg.yes']+'</option>' +
	        '<option value="false">'+J.msg['dataviewcfg.no']+'</option>';
	    '</select></div>';
		var $modify0 = $(modify0Html).appendTo($row);
		$modify0.tooltip({title:J.msg['dataviewcfg.modifyOnAddTooltip']});
		this.data && $modify0.find("." + this.componentClass).val(this.data.modify0===false?"false":"true");
		var modifyHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.modifyOnEdit']+':</label>' +
			'<select class="form-control c_modify ' + this.componentClass + '">' +
	    	'<option value="true" selected="selected">'+J.msg['dataviewcfg.yes']+'</option>' +
	        '<option value="false">'+J.msg['dataviewcfg.no']+'</option>';
	    '</select></div>';
		var $modify = $(modifyHtml).appendTo($row);
		$modify.tooltip({title:J.msg['dataviewcfg.modifyOnEditTooltip']});
		this.data && $modify.find("." + this.componentClass).val(this.data.modify===false?"false":"true");
		
		$row = $("<div class='row'></div>").appendTo($container);
		var cfgTypeHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.valType']+':</label>' +
			'<select class="form-control c_cfgType ' + this.componentClass + '">' +
	    	'<option value="string" selected="selected">string</option>' +
	        '<option value="json">json</option>';
	    '</select></div>';
		var $cfgType = $(cfgTypeHtml).appendTo($row);
		$cfgType.tooltip({title:J.msg['dataviewcfg.valTypeTooltip']});
		this.data && this.data.cfgType && $cfgType.find("." + this.componentClass).val(this.data.cfgType);
		var defaultValHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.defaultVal']+':</label>' +
			'<textarea class="form-control j-textarea-config c_defaultVal ' + this.componentClass + '"></textarea></div>';	
		var $defaultVal = $(defaultValHtml).appendTo($row);
		$defaultVal.tooltip({title:J.msg['dataviewcfg.defaultValTooltip']});
		this.data && this.data.defaultVal && $defaultVal.find("." + this.componentClass).val(this.data.defaultVal);
		
		$row = $("<div class='row'></div>").appendTo($container);
		var ruleHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.validateRule']+':</label>' +
			'<textarea class="form-control j-textarea-config c_rule ' + this.componentClass + '"></textarea></div>';	
		var $rule = $(ruleHtml).appendTo($row);
		$rule.tooltip({title:J.msg['dataviewcfg.validateRuleTooltip']});
		this.data && this.data.rule && $rule.find("." + this.componentClass).val(JSON.stringify(this.data.rule, 2));
		var sourceHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.source']+':</label>' +
			'<select class="form-control c_source ' + this.componentClass + '">' +
	    	'<option value="product" selected="selected">'+J.msg['dataviewcfg.sourcePm']+'</option>' +
	        '<option value="dev">'+J.msg['dataviewcfg.sourceDev']+'</option>';
	    '</select></div>';
		var $source = $(sourceHtml).appendTo($row);
		$source.tooltip({title:J.msg['dataviewcfg.sourceTooltip']});
		this.data && this.data.source && $source.find("." + this.componentClass).val(this.data.source);
		
		$row = $("<div class='row'></div>").appendTo($container);
		var valProcessHtml = 
			'<div class="form-group col-6"><label>'+J.msg['dataviewcfg.valProcess']+':</label>' +
			'<select class="form-control c_valProcess ' + this.componentClass + '">' +
	    	'<option value="key">'+J.msg['dataviewcfg.key']+'</option>' +
	        '<option value="keyPath">'+J.msg['dataviewcfg.keyPath']+'</option>' +
	        '<option value="self">'+J.msg['dataviewcfg.self']+'</option>' +
	    '</select></div>';
		var $valProcess = $(valProcessHtml).appendTo($row);
		$valProcess.tooltip({"title":J.msg['dataviewcfg.valProcessTooltip']});
		var renderValue = function(val) {
			$container.find(".data-conf").remove();
			if (val == 'key') {
				var html = '<div class="form-group col-6 data-conf"><label>'+J.msg['dataviewcfg.key']+':</label><input type="text" class="form-control c_cfgKey ' + this.componentClass + '"/></div>';
				$(html).insertAfter($valProcess);
			} else if (val == 'keyPath') {
				var html = '<div class="form-group col-3 data-conf"><label>'+J.msg['dataviewcfg.key']+':</label><input type="text" class="form-control c_cfgKey ' + this.componentClass + '"/></div>'
					+ '<div class="form-group col-3 data-conf"><label>'+J.msg['dataviewcfg.path']+':</label><input type="text" class="form-control c_cfgPath ' + this.componentClass + '"/></div>'
				$(html).insertAfter($valProcess);
			} else if (val == 'self') {
				var html = '<div class="form-group col-6 data-conf"><label>'+J.msg['dataviewcfg.script']+':</label><textarea class="form-control c_cfgSelf ' + this.componentClass + '"></textarea></div>';
				$(html).insertAfter($valProcess);
			}
			var $cfgKey = $container.find(".c_cfgKey." + this.componentClass);
			this.data && this.data.cfgKey && $cfgKey.val(this.data.cfgKey);
			var $cfgPath = $container.find(".c_cfgPath." + this.componentClass);
			this.data && this.data.cfgPath && $cfgPath.val(this.data.cfgPath);
			var $cfgSelf = $container.find(".c_cfgSelf." + this.componentClass);
			this.data && this.data.cfgSelf && $cfgSelf.val(this.data.cfgSelf);
		}
		var $valProcessSelect = $valProcess.children("select");
		renderValue.call(this, $valProcessSelect.val());
		$valProcessSelect.change(function(){
			renderValue.call(_self, $(this).val());
		});
		this.data && this.data.valProcess && $valProcessSelect.val(this.data.valProcess).change();

		$row = $("<div class='row'></div>").appendTo($container);
		var beforeRenderHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.beforeRender']+':</label>' +
			'<textarea class="form-control j-textarea-config c_beforeRender ' + this.componentClass + '"></textarea></div>';	
		var $beforeRender = $(beforeRenderHtml).appendTo($row);
		$beforeRender.tooltip({title:J.msg['dataviewcfg.beforeRenderTooltip']});
		this.data && this.data.beforeRender && $beforeRender.find("." + this.componentClass).val(this.data.beforeRender);
		var afterRenderHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.afterRender']+':</label>' +
			'<textarea class="form-control j-textarea-config c_afterRender ' + this.componentClass + '"></textarea></div>';	
		var $afterRender = $(afterRenderHtml).appendTo($row);
		$afterRender.tooltip({title:J.msg['dataviewcfg.afterRenderTooltip']});
		this.data && this.data.afterRender && $afterRender.find("." + this.componentClass).val(this.data.afterRender);

		var extHtml = 
			'<div class="form-group col"><label>'+J.msg['dataviewcfg.extConfig']+':</label>' +
			'<textarea class="form-control j-textarea-config c_ext ' + this.componentClass + '"></textarea></div>';	
		var $ext = $(extHtml).appendTo($row);
		$ext.tooltip({title:J.msg['dataviewcfg.extConfigTooltip']});
		this.data && this.data.ext && $ext.find("." + this.componentClass).val(this.data.ext);		
	}

	var _commonConfigCollect = function(data) {
		var ruleStr = this.$container.find(".c_rule." + this.componentClass).val();
		var rules = ruleStr?JSON.parse(ruleStr):null;
		var data = {required:this.$container.find(".c_require." + this.componentClass).val() == 'true',
				cfgType:this.$container.find(".c_cfgType." + this.componentClass).val(),
				defaultVal: this.$container.find(".c_defaultVal." + this.componentClass).val(),
				valProcess:this.$container.find(".c_valProcess." + this.componentClass).val(),
				cfgKey:this.$container.find(".c_cfgKey." + this.componentClass).val(),
				cfgPath:this.$container.find(".c_cfgPath." + this.componentClass).val(),
				cfgSelf:this.$container.find(".c_cfgSelf." + this.componentClass).val(),
				source:this.$container.find(".c_source." + this.componentClass).val(),
				modify0:this.$container.find(".c_modify0." + this.componentClass).val() == 'true',
				modify:this.$container.find(".c_modify." + this.componentClass).val() == 'true',
				desc:this.$container.find(".c_desc." + this.componentClass).val(),
				beforeRender:this.$container.find(".c_beforeRender." + this.componentClass).val(),
				afterRender:this.$container.find(".c_afterRender." + this.componentClass).val(),
				ext:this.$container.find(".c_ext." + this.componentClass).val(),
				rule:rules};
        data.cfgKey && (data.cfgKey=data.cfgKey.trim());
        data.cfgPath && (data.cfgPath=data.cfgPath.trim());
        if (data.ext) {
            try {
                eval(data.ext);
            } catch (err) {
                console.log("eval Exception, js:" + data.ext);
                console.dir(err);
                throw err;
            }
        }
		var newData = {};
		for (var key in data) {
			if (data[key] !== null && data[key] !== undefined && data[key] !== '') {
				newData[key] = data[key];
			}
		}
		return newData;
	}

	J.InputTextConfig = J.Components['inputTextConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'inputTextConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}
	J.TextAreaConfig = J.Components['textareaConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'textareaConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}
	J.SelfConfig = J.Components['selfConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'selfConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			var selfHtml = 
				'<div class="form-group col"><label>'+J.msg['dataviewcfg.selfScript']+':</label>' +
				'<textarea class="form-control selfScript ' + this.componentClass + '"></textarea></div>';	
			$(selfHtml).appendTo(this.$bodyContainer);
			var $selfScript = this.$bodyContainer.find("." + this.componentClass);
			this.data && this.data.selfScript && $selfScript.val(this.data.selfScript);
		}
		this.collect = function(data) {
			var collectData = {selfScript:_self.$bodyContainer.find(".selfScript." + this.componentClass).val()};
			return collectData;
		}
	}

	J.FixInputConfig = J.Components['fixInputConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'fixInputConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}

	J.SelectConfig = J.Components['selectConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'selectConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			var selectHtml = 
                '<div class="row">' +
				'<div class="form-group col-6"><label>'+J.msg['dataviewcfg.selectTypeSelect']+':</label><select class="form-control c_selectType ' + this.componentClass + '">' +
	            	'<option value="single" selected="selected">'+J.msg['dataviewcfg.single']+'</option>' +
	                '<option value="multiple">'+J.msg['dataviewcfg.multiple']+'</option>' +
	                '<option value="seqMultiple">'+J.msg['dataviewcfg.seqMultiple']+'</option>' +
	            '</select></div>' +
                '<div class="form-group col-6"><label>'+J.msg['dataviewcfg.selectValType']+':</label><select class="form-control c_selectValType ' + this.componentClass + '">' +
                    '<option value="" selected="selected">'+J.msg['dataviewcfg.default']+'</option>' +
                    '<option value="comma">'+J.msg['dataviewcfg.commaSep']+'</option>' +
                '</select></div>' +
                '</div>'              
                ;
                var $selectInfo = $(selectHtml).appendTo(this.$bodyContainer);

			var $selectType = $selectInfo.find("select.c_selectType");
			this.data && this.data.selectType && $selectType.val(this.data.selectType);

            var $selectValType = $selectInfo.find("select.c_selectValType");
            this.data && this.data.selectValType && $selectValType.val(this.data.selectValType); 
            
				var selectListConfig = {
					config : {
						insertRemovePosition:"Right",
						component : {
							type : function(cfg) {
								J.BaseComponent.call(this, 'selectOption', cfg);
								this.render=function($container){
									var listHtml = 
					                    '<div class="row c_option ' + _self.componentClass + '">' +
			                                '<div class="form-group col">' +
			                                     '<label>'+J.msg['dataviewcfg.optionName']+':</label><input type="text" class="form-control c_optionName" value="' + (this.data&&this.data.name?this.data.name:"")+'"/>' +
			                                '</div>' +
			                                '<div class="form-group col">' +
			                                    '<label>'+J.msg['dataviewcfg.optionValue']+':</label><input type="text" class="form-control c_optionValue"/>' +
			                                '</div>' +
			                            '</div>';
										var $listHtml = $(listHtml).appendTo($container);
										$listHtml.find(".c_optionValue").val(this.data&&this.data.value?this.data.value:"");
								}
							}
						}
					},
					allConfig:cfg.allConfig, data:_self.data.selectList, allData:cfg.allData
				};
			var selectList = new J.List(selectListConfig);
			var $selectListCol = $("<div class='col'></div>").appendTo(this.$bodyContainer);
			selectList.render($selectListCol);
			_commonConfigRender.call(_self, this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			collectData.selectType = this.$container.find(".c_selectType." + this.componentClass).val();
			collectData.selectValType = this.$container.find(".c_selectValType." + this.componentClass).val();
			var $options = this.$container.find(".c_option." + this.componentClass);
			var options = [];
			$options.each(function(){
				var optionName = $(this).find(".c_optionName").val();
				var optionValue = $(this).find(".c_optionValue").val();
				options.push({name:optionName, value:optionValue});
			});
			collectData.selectList = options;
			return collectData;
		}
	}

	J.TableSelectConfig = J.Components['tableSelectConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'tableSelectConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}

	J.ListSelectConfig = J.Components['listSelectConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'listSelectConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}

	J.CardListConfig = J.Components['cardListConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'cardListConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
			var cardConfig = {
	                "component": {
	                    "type": "list",
	                    "addText":J.msg['dataviewcfg.addConfigItem'],
	                    "insertRemovePosition":"Right",
	                    "component": {
	                        "type": "elementConfig"
	                    }
	                }
			};
			this.elementCard = new J.CardConfig({config:cardConfig,allConfig:cardConfig,data:cfg.data&&cfg.data.component,allData:cfg.allData,ctx:cfg.ctx});
			var $extRow = $('<div class="row j-element-ext"></div>').insertAfter($container.parent());
			this.elementCard.render($extRow);
			this.elementCard.$cardConfig.addClass("col");
			
	    }
		
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			collectData.addText=collectData.addText ||J.msg['dataviewcfg.add'];
			collectData.insertRemovePosition= collectData.insertRemovePosition || "Right";
			var elementCardData = this.elementCard.collect({});
			collectData.component = elementCardData;
			return collectData;
		}	
	}
	
    J.TableConfig = J.Components['tableConfig'] = function(cfg) {
        J.BaseComponent.call(this, 'tableConfig', cfg);
        var _self = this;
        this.render = function($container) {
            this.$container = $container;
            _commonAddConfigBody.call(this, $container);
            _commonConfigRender.call(_self,this.$bodyContainer);
        }
        this.collect = function(data) {
            var collectData = _commonConfigCollect.call(_self, data);
            return collectData;
        }
    }
	
	J.ElementsCardConfig = J.Components['elementsCardConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'elementsCardConfig', cfg);
		var _self = this;		
		this.render = function($container) {
			this.$container = $container;
			_commonAddConfigBody.call(this, $container);
			_commonConfigRender.call(_self,this.$bodyContainer);
			var cardConfig = {
		            "component": {
		                "type": "list",
		                "addText":J.msg['dataviewcfg.addConfigItem'],
		                "insertRemovePosition":"Right",
		                "component": {
		                    "type": "elementConfig"
		                }
		            }
			};
			this.elementCard = new J.CardConfig({config:cardConfig,allConfig:cardConfig,data:cfg.data&&cfg.data.component,allData:cfg.allData,ctx:cfg.ctx});
			var $extRow = $('<div class="row j-element-ext"></div>').insertAfter($container.parent());
			this.elementCard.render($extRow);
			this.elementCard.$cardConfig.addClass("col");
			
		}
		
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			var elementCardData = this.elementCard.collect({});
			collectData.component = elementCardData;
			return collectData;
		}			
	}

   })(J);
