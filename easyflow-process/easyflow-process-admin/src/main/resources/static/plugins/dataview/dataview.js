(function (J) {
	J.DataView={};
	J.Components={};

	var _createComponent = function(type, config) {
		if (typeof type == 'string') {
			return new J.Components[type](config);
			} else {
			return new type(config);
		}
	}
	J.BaseComponent = function(type, cfg) {
		console.log('Init component' + type);
		this.type = type;
		this.cfg = cfg;
		this.config = cfg.config;
		this.allConfig = cfg.allConfig;
		this.data = cfg.data;
		this.allData = cfg.allData;
		this.originData = cfg.originData;
		this.ctx = cfg.ctx;
		this.$container = cfg.$container;
		this.component = null;
		this.id = $.jSequence.next();
		this.name='n_' + this.id;
		
		this.componentClass = "c_" + this.id;
		if (cfg.config && cfg.config.component && this.createComponent !== false) {
			var config = {config:cfg.config.component, allConfig:cfg.allConfig, data:this.componentData, allData:cfg.allData, ctx:cfg.ctx};
			this.component = _createComponent.call(this, cfg.config.component.type, config);
		}
		this.components = [];
		if (cfg.config && cfg.config.components) {
			for (var i in cfg.config.components) {
				var config = {config:cfg.config.components[i], allConfig:cfg.allConfig, data:this.componentsData && this.componentsData[i], allData:cfg.allData, ctx:cfg.ctx};
				var type = cfg.config.components[i].type;
				    this.components[i] = _createComponent.call(this, type, config);
				}
		}

		this.renderComponents = function($container) {
			for (var i in this.components) {
				this.components[i].render($container);
			}
		}
		this.collect = function(data){
			console.log("collect " + this.type)
			_beforeCollect.call(this, data);
			if (this.component) {
				this.component.collect(data);
			}
			for (var i in this.components) {
				this.components[i].collect(data);
			}
			_afterCollect.call(this, data);
			return data;
		}
		
		var _beforeCollect = function(data) {
			var beforeCollect = this.config.beforeCollect;
			if (beforeCollect) {
                try {
				eval(beforeCollect);
                } catch (err) {
                    console.log("eval exception, js:" + beforeCollect);
                    console.dir(err);
                    throw err;
                }
			}
		}
		var _afterCollect = function(data) {
			var afterCollect = this.config.afterCollect;
			if (afterCollect) {
                try {
				eval(afterCollect);
                } catch (err) {
                    console.log("eval Exception, js:" + afterCollect);
                    console.dir(err);
                    throw err;
                }				
			}
		}
	}

	J.List = J.Components['list'] = function(cfg) {
		this.createComponent = false;
		J.BaseComponent.call(this, 'list', cfg);
		this.render = function($container) {
			console.log("Render list");
			var addText = this.config.addText ? this.config.addText:J.msg['dataview.add'];
			var listHeadHtml = this.config.name ? '<div class="alert alert-secondary">'+this.config.name +'</div>':'';
			var html = '<div class="j-list">' +
			listHeadHtml +
	        '<div class="j-list-body">' +
	        '</div>' +
	        '<div class="j-list-footer mt-1">' +
	        '<div class="row"><div class="col"><button type="button" class="btn btn-secondary btn-block add">' + addText + '</button></div></div>' +
	        '</div>' +
	        '</div>';
			var $list = this.$list = $(html).appendTo($container);
		    var insertRemovePosition = this.config.insertRemovePosition ? this.config.insertRemovePosition : 'Bottom';
		    var elHtml = null;
		    if (insertRemovePosition == 'Bottom') {
		    	elHtml = '<div class="j-list-el">' +
		        '        <div class="j-list-el-body ' + this.componentClass + '"></div>' +
		        '        <div class="j-list-el-footer">' +
		        '            <div class="row"><div class="col"><button type="button" class="btn btn-light btn-block insert">+</button></div><div class="col"><button type="button" class="btn btn-light btn-block remove">-</button></div></div>' +
		        '        </div>' +
		        '    </div>';	
		    } else if (insertRemovePosition == 'Right'){
		    	elHtml = '<div class="j-list-el row">' +
		        '        <div class="j-list-el-body col ' + this.componentClass + '"></div>' +
		        '        <div class="j-list-el-footer col-1">' +
		        '            <div class="row"><div class="col"><button type="button" class="btn  btn-light btn-block insert"><i class="fa fa-plus"></i></button></div><div class="col"><button type="button" class="btn  btn-light btn-block remove"><i class="fa fa-trash"></i></button></div></div>' +
		        '        </div>' +
		        '    </div>';
		    } else {
		    	elHtml = '<div class="j-list-el row">' +
		        '        <div class="j-list-el-body col ' + this.componentClass + '"></div>' +
		        '    </div>';		    	
		    }
			var renderEl = function($el, data){
				var component = _createComponent.call(this, cfg.config.component.type, {config:cfg.config.component, allConfig:cfg.allConfig, data:data, allData:data, ctx:cfg.ctx});
				component.render($el);
				$el.data('component', component);
			};
			
			$list.on("click",".remove", function(){
				if (!$list.is($(this).parents(".j-list")[0])) {
					return;
				}
				$($(this).parents('.j-list-el')[0]).remove();
			})
			.on("click",".insert", function(){
				if (!$list.is($(this).parents(".j-list")[0])) {
					return;
				}
				$el = $(elHtml).insertAfter($(this).parents('.j-list-el')[0]).find(".j-list-el-body");
				renderEl($el,{});
			})
			.on("click", ".add", function(){
				if (!$list.is($(this).parents(".j-list")[0])) {
					return;
				}
				$el = $(elHtml).appendTo($list.children(".j-list-body")).find(".j-list-el-body");
				renderEl($el,{});
			});
			if (this.data) {
				for (var i in this.data) {
					$el = $(elHtml).appendTo($list.children(".j-list-body")).find(".j-list-el-body");
					renderEl($el, this.data[i]);
				}
			}
			
		}
		this.collect = function(data) {
			var listData = [];
			this.$list.find("." + this.componentClass).each(function(i){
				var component = $(this).data("component");
				var subData = {};
				var subData = component.collect(subData);
				listData.push(subData);
			});
			if (listData.length == 0) {
				listData = null;
			}
			return listData;
		}
	}


J.jsonTemplateConfig = {
		  "type": "page",
		  "component": {
		            "type": "panel",
		            "name": J.msg["dataview.dataInfo"],
		            "components": [
		              {
		                "type": "card",
		                "name": "",
		                "cols": 1,
		                "components": [
		                  {
		                    "required": true,
		                    "cfgType": "json",
		                    "valProcess": "self",
		                    "cfgSelf": "function _selfFunc(type){\nif (type=='parse'){\nreturn this.allData;\n} else {\nvar val = this.$element.val();\nvar valObj = JSON.parse(val);\nfor(var key in data){delete data[key]};\nObject.assign(data, valObj);\n}\n};\n_selfFunc",
		                    "source": "product",
		                    "modify0": true,
		                    "modify": true,
		                    "type": "textarea",
		                    "name": J.msg["dataview.originalData"],
		                    "cols": -1,
		                    "newline": false
		                  }
		                ]
		              }
		            ]
		  },
		  "beforeRender": "",
		  "afterRender": ""
		};

   })(J);
