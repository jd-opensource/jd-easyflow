(function (J) {
	
	J.DataView.additionalConfigTypeList=[
		{value: "dateInput", "text":J.msg['dataviewcfg.dateType']},
		{value: "fileInput", "text":J.msg['dataviewcfg.fileType']},
		{value: "mask", "text":J.msg['dataviewcfg.maskInputType']},
		{value: "flow", "text":J.msg['dataviewcfg.flowType']},
		{value: "fsm", "text":J.msg['dataviewcfg.fsmType']}
	];
	
	J.FlowConfig = J.Components['flowConfig'] = J.FsmConfig = J.Components['fsmConfig']=J.InputTextConfig;
	J.Flow = J.Components['flow'] = function(cfg) {
		J.BaseComponent.call(this, 'flow', cfg);
		var _self = this;	
		J._commonInit.call(this);
		this.innerRender=function($container){
			this.$input.jFlowInput({name:this.name, clazz:this.componentClass});
            this.$element = this.$input.data("jFlowInput").jTableSelect.$inputField;
		}
	}

	J.Fsm = J.Components['fsm'] = function(cfg) {
		J.BaseComponent.call(this, 'fsm', cfg);
		var _self = this;	
		J._commonInit.call(this);
		this.innerRender=function($container){
			this.$input.jFsmInput({name:this.name, clazz:this.componentClass});
            this.$element = this.$input.data("jFsmInput").jTableSelect.$inputField;
		}
	}

    J.MaskConfig = J.Components['maskConfig'] = J.InputTextConfig;
    J.Mask = J.Components['mask'] = function(cfg) {
        J.BaseComponent.call(this, 'mask', cfg);
        var _self = this;
        J._commonInit.call(this);
        this.innerRender=function($container){
            var $element = $('<input type="text" class="form-control ' + this.componentClass + '" name="' + this.name + '"/>').appendTo(this.$input);
            $element.jMaskInput({"plaintextFunc":this.plaintextFunc});            
        }
    }   
	
	
	J.DateInputConfig = J.Components['dateInputConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'dateInputConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			J._commonAddConfigBody.call(this, $container);
			J._commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			return collectData;
		}
	}
	
	J.DateInput = J.Components['dateInput'] = function(cfg) {
	    J.BaseComponent.call(this, 'dateInput', cfg);
	    var _self = this;
	    J._commonInit.call(this);
	    this.innerRender=function($container){
	        $('<input type="text" class="form-control ' + this.componentClass + '" name="' + this.name + '" onchange="$(this).valid()"/>').appendTo(this.$input);
	        this.$element=this.$input.find("." + this.componentClass);
	        var readonly = false;
	        if ((!this.ctx || this.ctx.op =='add') && ! this.config.modify0) {
	            readonly = true;
	        }
	        if (this.ctx && this.ctx.op =='modify' && ! this.config.modify) {
	            readonly = true;
	        }
	        var conf = this.dateInputConf;//compute from beforeRender
	        if (! readonly) {
	            this.$element.datetimepicker(conf);
	        }
	    }
	}
	
	J.FileInputConfig = J.Components['fileInputConfig'] = function(cfg) {
		J.BaseComponent.call(this, 'fileInputConfig', cfg);
		var _self = this;
		this.render = function($container) {
			this.$container = $container;
			J._commonAddConfigBody.call(this, $container);
			var multipleHtml = 
				'<div class="form-group col-6"><label>'+J.msg['dataviewcfg.fileCount']+':</label><select class="form-control c_multiple ' + this.componentClass + '">' +
	            	'<option value="single" selected="selected">'+J.msg['dataviewcfg.singleFile']+'</option>' +
	                '<option value="multiple">'+J.msg['dataviewcfg.multipleFile']+'</option>' +
	            '</select></div>';
			var $multiple = $(multipleHtml).appendTo(this.$bodyContainer).find("select");
			this.data && this.data.multiple && $multiple.val(this.data.multiple);
			J._commonConfigRender.call(_self,this.$bodyContainer);
		}
		this.collect = function(data) {
			var collectData = _commonConfigCollect.call(_self, data);
			collectData.multiple = this.$container.find(".c_multiple." + this.componentClass).val();
			return collectData;
		}
	}
	
	J.FileInput = J.Components['fileInput'] = function(cfg) {
	    J.BaseComponent.call(this, 'fileInput', cfg);
	    var _self = this;
	    J._commonInit.call(this);
	    this.innerRender=function($container){
	        var multipleHtml = this.config.multiple == "multiple" ? ' multiple="multiple"':"";
	        $('<input type="file" class="form-control ' + this.componentClass + '" name="' + this.name + '"' + multipleHtml + '/>').appendTo(this.$input);
	        this.$element=this.$input.find("." + this.componentClass);
	        if ((!this.ctx || this.ctx.op =='add') && ! this.config.modify0) {
	            this.$element.attr("readonly", "readonly");
	        }
	        if (this.ctx && this.ctx.op =='modify' && ! this.config.modify) {
	            this.$element.attr("readonly", "readonly");
	        }

	        var conf = this.fileInputConf;//computed from before render
	        this.$element.fileinput(conf);
	        // after

	    }
	    this.innerVal = function(){}
	    this.collect = function(data) {
	        var preview = this.$element.fileinput("getPreview");
	        var previewConfig = preview.config;
	        var keys = [];
	        for (var i in previewConfig){
	            keys.push({key:previewConfig[i].key, caption:previewConfig[i].caption, size:previewConfig[i].size});
	        }
	        if (keys.length == 0){
	            keys = null;
	        }
	        J._commonCollect.call(this, JSON.stringify(keys), data);
	    }
	    }	
	
		
	
})(J);