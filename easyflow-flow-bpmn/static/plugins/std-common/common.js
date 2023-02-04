if (! window.J) {
  window.J = {};
}
(function (J) {
// bootstrap datatable plugin default setting.
$.fn.bootstrapTable && $.extend($.fn.bootstrapTable.defaults, {
    pagination : true,
    pageList:[10,25,100],
    sidePagination : "server",
    locale : J.msg['locale'],
    cache : false,
    clickToSelect : true,
    showRefresh : true,
    showColumns : true,
    queryParamsType: "",
    escape:true,
    ajaxOptions : {
        traditional: true
    },
    responseHandler : function(res) {
        return {
            total : res.resultData.count,
            rows : res.resultData.list
        }
    },
});

// jquery datetimepicker default setting
$.fn.datetimepicker && ($.fn.datetimepicker.defaults={
        fontAwesome:true,
        format : "yyyy-mm-dd",
        minView : "month",
        autoclose : 1,
        language:J.msg['locale'],
        todayBtn:true
})

// bootstrap multiselect default setting
$.fn.multiselect && $.extend($.fn.multiselect.Constructor.prototype.defaults, {
    nonSelectedText:J.msg['multiselect.nonSelectedText'],
    nSelectedText:J.msg['multiselect.nSelectedText'],
    allSelectedText:J.msg['multiselect.allSelectedText:msg'],
    filterPlaceholder:J.msg['multiselect.filterPlaceholder'],
    selectAllText:J.msg['multiselect.selectAllText'],
    includeSelectAllOption: true,
    buttonWidth:"100%",
    filterBehavior:"both",
    numberDisplayed:2,
    maxHeight: 200
});



/*Get form original data*/
   $.fn.serializeObject = function() {
       "use strict";
       var result = {};
       var extend = function(i, element) {
           var node = result[element.name];
           if ('undefined' !== typeof node && node !== null) {
               if ($.isArray(node)) {
                   node.push(element.value);
               } else {
                   result[element.name] = [node, element.value];
               }
           } else {
               result[element.name] = element.value;
           }
       };

       $.each(this.serializeArray(), extend);
       return result;
   };
// ====jquery validate====  
   if(jQuery.validator) {
        $.validator.setDefaults({
            ignore:"",
            errorPlacement: function(error, element) {
                if(element.is(".dtselect")) {
                    error.insertAfter(element.parent());
                } else {
                    error.insertAfter(element); 
                }
            },
            onfocusout: function( element ) {
                if (!this.checkable( element ) && "file" != element.type) {
                    this.element( element );
                }
            }
            });
        /**Date validate*/
        jQuery.validator.addMethod("strictDate", function(value, element, param) {
            if (value == null || value=="") {
                return true;
            }
            var r = value.match(/^(\d{1,4})(-|\/)(\d{1,2})\2(\d{1,2})$/);
            if(r==null) return false;
            var d = new Date(r[1], r[3]-1,r[4]);
            return (d.getFullYear()==r[1]&&(d.getMonth()+1)==r[3]&&d.getDate()==r[4]);
        }, $.validator.format(J.msg['jqueryValidate.strictDate']));
        /**JSON format validate*/
        jQuery.validator.addMethod("json", function(value, element) {
            if (value == null || value=="") {
                return true;
            }
            try {
                var obj=JSON.parse(value);
                return typeof obj == 'object' && obj;
            } catch (err) {
                console.log("not json:" + value);
                return false;
            }
        }, $.validator.format(J.msg['jqueryValidate.json']));     
        
        jQuery.validator.addMethod("fileinputRequiredByPreview", function(value, element) {
            var preview = $(element).fileinput("getPreview");
            if (preview != null && preview.config!=null && preview.config.length>0) {
                return true;
            }
            return false;
            
        }, $.validator.format(J.msg['jqueryValidate.fileInput']));          

    jQuery.extend(jQuery.validator.messages, {
        required: J.msg["jqueryValidate.required"],
        remote: J.msg["jqueryValidate.remote"],
        email: J.msg["jqueryValidate.email"],
        url: J.msg["jqueryValidate.url"],
        date: J.msg["jqueryValidate.date"],
        dateISO: J.msg["jqueryValidate.dateISO"],
        number: J.msg["jqueryValidate.number"],
        digits: J.msg["jqueryValidate.digits"],
        creditcard: J.msg["jqueryValidate.creditcard"],
        equalTo: J.msg["jqueryValidate.equalTo"],
        accept: J.msg["jqueryValidate.accept"],
        maxlength: jQuery.validator.format(J.msg["jqueryValidate.maxlength"]),
        minlength: jQuery.validator.format(J.msg["jqueryValidate.minlength"]),
        rangelength: jQuery.validator.format(J.msg["jqueryValidate.rangelength"]),
        range: jQuery.validator.format(J.msg["jqueryValidate.range"]),
        max: jQuery.validator.format(J.msg["jqueryValidate.max"]),
        min: jQuery.validator.format(J.msg["jqueryValidate.min"])
    });
    }
    
    // Get Current date
    J.getNowDate=function(sep1) {
        var date = new Date();
            var seperator1 = sep1!==undefined ? sep1 : "-";
        var month = date.getMonth() + 1<10? "0"+(date.getMonth() + 1):date.getMonth() + 1;
        var strDate = date.getDate()<10? "0" + date.getDate():date.getDate();
        var currentdate = date.getFullYear() + seperator1  + month  + seperator1  + strDate;
        return currentdate;
    }

    J.getNowDatetime=function(sep1, sep2, sep3) {
            var date = new Date();
            var seperator1 = sep1!==undefined ? sep1 : "-";
            var seperator2 = sep2!==undefined ? sep2 : " ";
            var seperator3 = sep3!==undefined ? sep3 : ":";
            var month = date.getMonth() + 1<10? "0"+(date.getMonth() + 1):date.getMonth() + 1;
            var strDate = date.getDate()<10? "0" + date.getDate():date.getDate();

            var hours = date.getHours()<10? "0" + date.getHours():date.getHours();
            var min  = date.getMinutes()<10? "0" + date.getMinutes():date.getMinutes();
            var seconds = date.getSeconds()<10? "0" + date.getSeconds():date.getSeconds();
            var currentdate = date.getFullYear() + seperator1  + month  + seperator1  + strDate  + seperator2 + hours + seperator3 + min + seperator3 + seconds;
            return currentdate;
    }

   })(window.J);
   
// ====JTabs====
$.fn.jTabs=function(option, extOption) {
    // Create tab
    if (typeof option !== 'string') {
        // Construct tabs
        this.append('<ul class="nav nav-tabs" id="tab" role="tablist" style="display:none"></ul>')
            .append('<div class="tab-content" id="tab-content"></div>');

    } else {
        if (option == 'open') {
            // Open tab
            var tabId = (extOption.id).replace(/\./g,"");
            var $tabs = this.find(".nav-tabs");
            var $content = this.find(".tab-content");
            var pannelId = "pannel_" + tabId;
            if ($("#"+tabId).length==0) {
                var tabName = extOption.name ? extOption.name : extOption.id;
                var closeBtn = extOption.closeable!==false ? "<button type='button' class='close'><span>&times;</span></button>":"";
                $tabs.append("<li class='nav-item'><a class='nav-link' id='" + tabId +"' data-toggle='tab' href='#" + pannelId +  "' role='tab'>"+tabName+closeBtn + "</a></li>");
                $content.append("<div class='tab-pane' id='" +  pannelId + "' role='tabpanel'></div>");
                // element is jquery type.
                if (extOption.element != null) {
                    $("#"+pannelId).append(extOption.element);
                // ajax url type
                } else {
                $("#"+pannelId).load(extOption.url);
                }
            }
            if ($tabs.children(".nav-item").length>1) {
                $tabs.show();
            }
            $("#"+tabId).tab('show');
        }
    }
    return this;
}
    
    // tab close button
$(function(){
    $("body").on("click", ".nav-tabs .close", function(){
    var tab = $(this).parent().parent();
    var pannel = $($(this).parent().attr("href"));
    var tabs = tab.parent();
    var active = $(this).parent().hasClass("active");
    tab.remove();
    pannel.remove();
    if (active) {
        tabs.find("[role='tab']:eq(0)").tab("show");
    }
      if(tabs.children("li").length==1) {
          tabs.hide();
       }
    
  });
});

// ajax success callback
var _ajaxSuccessCallback = function(data, option, $element) {
      var resCode = data.resultCode;
      var showMsg = data.resultMsg;
      var extData = data.extData;
      var successUrl = option.successUrl;
      if (data.extData && data.extData.successUrl) {
            successUrl = data.extData.successUrl;
      }
      // Success response
      if (resCode == null || resCode == "0000000") {          
          // Tip success in page.
          if (showMsg === null || showMsg === '') {
              showMsg = J.msg['ajaxSuccessCallback.successShowMsg'];
          }
         $.jPageTip(showMsg);
         // redirect URL
         if (successUrl != null) {
             window.location.href=successUrl;
             return;
         }
         // Close dialog
         var $dialog = $element != null ? $element.parents(".modal"):null;
         if ($dialog!=null && $dialog.length > 0) {
           $dialog.modal('hide').modal("dispose").remove();
         }
         // Refresh table
         var $bstable = $("body").find(".bootstrap-table");
         if ($bstable.length > 0) {
             $bstable.find(".table").bootstrapTable("refresh");
         }
      // Fail response
      } else if (resCode = '0000002') {
        if (showMsg != null) {
            $.jMessage({
                title : '&nbsp;',
                msg : showMsg
            });
        }
        // Has error
        var fieldErrors = extData == null ? null : extData["fieldErrors"];
        if (fieldErrors != null) {
            for (var i = 0; i < fieldErrors.length; i++) {
                $.jFormTip($element, fieldErrors[i].field, fieldErrors[i].msg);
            }
        }
    } else {
        // show exception in dialog
        if (showMsg === null || showMsg === '') {
            showMsg = J.msg['ajaxSuccessCallback.exceptionShowMsg'];;
        }
        $.jMessage({
            title : '&nbsp;',
            msg : showMsg
        });
    }
      
  };
  
  
  $.extend({
        getNowDate:function() {
            var date = new Date();
            var seperator1 = "-";
            var month = date.getMonth() + 1<10? "0"+(date.getMonth() + 1):date.getMonth() + 1;
            var strDate = date.getDate()<10? "0" + date.getDate():date.getDate();
            var currentdate = date.getFullYear() + seperator1  + month  + seperator1  + strDate;
            return currentdate;
        },
        getBaseUrl: function () {
            if ($._baseUrl) return $._baseUrl;
            var commonCss = '/static/css/common.css';
            var scriptTags = document.getElementsByTagName('link');
            if (scriptTags.length <= 0) return;
            for (var i = 0; i < scriptTags.length; i++) {
                var src = scriptTags[i].href;
                if (src.indexOf(commonCss) > -1) {
                    $._baseUrl = src.substring(0, src.indexOf("/static"));
                    return $._baseUrl;
                }
            }
        },
        jSequence : {
                   current: 1000,
                   next: function () {
                       return this.current++;
                   }
         },
      //====Config====
      jConfig : {
                loading : {
                    top:45,
                    left:225
                }   
            },   
     //====Loading====
        jLoading: {
            entity: null,
            count : 0,
            show: function () {
                if (this.entity == null) {
                    $("body").append("<div id='jloading'><i class='fa fa-spinner fa-pulse' style='font-size:5rem'></i></div>");
                    this.entity = $("#jloading");
                }
                var top = $.jConfig.loading.top;
                var left = $.jConfig.loading.left;
                this.entity.css({top:top, left: left, width: $("body").width() - left, height: $("body").height() - top, background: "none"});
                this.entity.find("i").hide();
                this.entity.show();

                var _self=this;
                var pointer = {};
                this.count++;
                var timer = setTimeout(function () {
                    _self.entity.find("i").css({top: 200, left: ($(window).width() - $.jConfig.loading.left - 100) / 2}).show();
                    _self.entity.css({background: "#FFFFFF"}).fadeTo("fast", 0.3);
                }, 1000);
                pointer.timer = timer;
                return pointer;
            },
            hide: function (pointer) {
                clearTimeout(pointer.timer);
                this.count--;
                if(this.count == 0) {
                    this.entity.hide();
                }
            }
        },  
      // JMsg
      jMessage:function(option){
          var popId = "pop_" + $.jSequence.next();
          var title = option.title ? option.title : "&nbsp;";
          $("body").append("<div id='" + popId + "' class='modal' tabindex='-1' role='dialog'>" + 
                              "<div class='modal-dialog' role='document'>" + 
                                "<div class='modal-content'>" + 
                                   "<div class='modal-header'><h5 class='modal-title'>" + title + "</h5><button type='button' class='close' data-dismiss='modal' aria-label='Close'><span>&times;</span></button></div>" +
                                   "<div class='modal-body'><p>" + option.msg + "</p></div>" + 
                                   "<div class='modal-footer'><button type='button' class='btn btn-secondary' data-dismiss='modal'>" + J.msg['jDialog.close'] + "</button></div>" + 
                                "</div>" + 
                               "</div>" + 
                            "</div>");
          var $element = this.$element = $("#" + popId);
          this.$element.modal().on("hidden.bs.modal", function(){
              $element.modal("dispose");
              $element.remove();
          }); 
      },
      // ====JConfirm====
      jConfirm:function(option){
          var popId = "pop_" + jQuery.jSequence.next();
          var title = option.title ? option.title : "";
          $("body").append("<div id='" + popId + "' class='modal' tabindex='-1' role='dialog'>" + 
                  "<div class='modal-dialog' role='document'>" + 
                    "<div class='modal-content'>" + 
                       "<div class='modal-header'><h5 class='modal-title'>" + title + "</h5><button type='button' class='close' data-dismiss='modal' aria-label='Close'><span>&times;</span></button></div>" +
                       "<div class='modal-body'><p>" + option.msg + "</p></div>" + 
                       "<div class='modal-footer'><button type='button' class='btn btn-primary j-btn-ok'>" + J.msg['jDialog.confirm'] + "</button><button type='button' class='btn btn-secondary' data-dismiss='modal'>" + J.msg['jDialog.cancel'] + "</button></div>" + 
                    "</div>" + 
                   "</div>" + 
                "</div>");
        var $element = $("#" + popId);
        $element.modal();
        $element.on("hidden.bs.modal", function(){
            $element.modal("dispose").remove();
        });
         $("#" + popId + " .j-btn-ok").click(function () {
             $element.modal("hide");
           if(option.callback) {
            option.callback.call(window);
           }
       });  
      },
      // ====JDialog====
    jDialog: function(option) {
        var title = option.title;
        var size = option.size ? option.size : "modal-xl";
        var $dialog = $('<div class="modal" tabindex="-1" role="dialog">' + 
                  '<div class="modal-dialog  modal-dialog-scrollable ' + size + '" role="document" >' +
                    '<div class="modal-content">' + 
                      '<div class="modal-header">' + 
                        '<h5 class="modal-title">' + option.title + '</h5>' +
                        '<button type="button" class="close" data-dismiss="modal" aria-label="Close">' +
                          '<span aria-hidden="true">&times;</span>' + 
                        '</button>' + 
                      '</div>' + 
                      '<div class="modal-body">' + 
                      '</div>' +
                    '</div>' +
                  '</div>' + 
                '</div>').appendTo($("body"));
        if(option.element != null) {
        $dialog.find(".modal-body").append(option.element);
        option.success && option.success.call($dialog);
        $dialog.modal("handleUpdate");
        } else if (option.url != null) {
            $dialog.find(".modal-body").load(option.url, function(response){
                option.success && option.success.call($dialog);
                $dialog.modal("handleUpdate");
            });
        }
          $dialog.on("hidden.bs.modal", function(){
            $dialog.modal("dispose");
            $dialog.remove();
          });
       $dialog.on("click", ".j-btn-cancel", function(){
           $dialog.modal("hide");
       });
       if (option.action) {
           option.action.call(this, $dialog);
       }
        $dialog.modal();
        
    },
    // Compare dialog
    jDiffDialog : function(option) {
        var element = `
<div class="row">
    <div class="col j-diff-left"><label>${option.left.title}:</label><textarea class="bpmnDef form-control" rows="30" readonly="readonly"></textarea></div>
    <div class="col j-diff-right"><label>${option.right.title}:</label><textarea class="bpmnDef form-control" rows="30" readonly="readonly"></textarea></div>
</div>        
        `;
        option.element = element;
        option.action = ($dialog)=>{
            $dialog.find(".j-diff-left textarea").text(option.left.content);
            $dialog.find(".j-diff-right textarea").text(option.right.content);
        }
        $.jDialog(option);
    },
      //====JFormTip====
      jFormTip:function () {
            if (arguments.length == 2) {
                var form = arguments[0];
                var tips = arguments[1];
                $.jMessage({msg: tips});
            } else {
                var form = arguments[0];
                var field = arguments[1];
                var tips = arguments[2];
                var error = {};
                error[field] = tips;
                form.validate().showErrors(error);
            }
        },
     //====jPageTip====
    jPageTip:function(tips){
        var windowWidth = document.documentElement.clientWidth;
        var $element = $("body").find(".jtip");
        if ($element.length==0) {
            $element = $("<div class='j-tip'><span class='j-tip-content'></span></div>").appendTo("body");
            $element.css({display: "none", 'top': 100 + 'px', 'left': ( windowWidth / 2 ) + 'px'});
        }
        $element.find(".j-tip-content").html(tips);
        $element.show()
        .fadeTo("fast", 1.0, function () {
            setTimeout(function () {
                $element.fadeTo("slow", 0, function () {
                    $(this).hide();
                });
            }, 2000);
        });
    },
    //====jAjax
    jAjax : function (option, $element) {
        var confirm = option.confirm;
        
        var type = option.type;
        if (! type) {
            type="GET";
        }
        var loading = null;
        
        var ajaxCall = function() {
            loading = $.jLoading.show();
            $.ajax({
                cache: false,
                type: type,
                url: option.url,
                data: option.data,
                traditional: true,
                dataType: "text",
                success: function (data) {
                    data = (data == null || data == '') ? {}:$.parseJSON(data);
                    $.jLoading.hide(loading);
                    if (option.success) {
                        option.success.call(window, data);
                    } else {
                        _ajaxSuccessCallback.call(window, data, option, $element);
                    }
                },
                error: function () {
                    $.jLoading.hide(loading);
                    $.jMessage({title: '&nbsp;', msg: J.msg['jAjax.requestException']});
                }
            });
        };
        
           // Has confirm dialog
           if (confirm) {
             $.jConfirm({msg:confirm, callback:function(){
                ajaxCall();
            }});
           // No confirm dialog
           } else {
            ajaxCall();
           }
    }
  });
  //====jAjaxSubmit====
  $.fn.jAjaxSubmit=function(option) {
      var $form = this;
      option = option == null ? {}:option;
      var successUrl = $form.data("success-url");
      option.successUrl = successUrl;
      var confirm = $form.data("confirm");
      
      var fileInputs = $('input[type=file]', $form);
      var hasFileInputs = fileInputs.length > 0;
      if(hasFileInputs && (option.iframe === undefined)) {
        option.iframe=true;
      }
      option.dataType = "text";
      
      var loading = null;
      // Success callback
      var callback = option.callback;
      if (! callback) {
          // Default callback process
          callback = _ajaxSuccessCallback;
      }
      option.success = function (data) {
           $.jLoading.hide(loading);
           data = (data == null || data == '') ? {}:$.parseJSON(data);
           callback.call(window, data, option, $form);
       };
       // Fail callback
      option.error = function () {
           $.jLoading.hide(loading);
           $.jMessage({title: '&nbsp;', msg: J.msg['jAjax.requestException']});
       };
  
       // Has confirm dialog
       if (confirm) {
         $.jConfirm({msg:confirm, callback:function(){
            loading = $.jLoading.show();
            $form.ajaxSubmit(option);
        }});
       // No confirm dialog
       } else {
        loading=$.jLoading.show();
        $form.ajaxSubmit(option);
       }
  };
  
//Table select control
  var JTableSelect=function(option) {
        this.$container = option.container;
        this.fieldValueName=option.name;//form field name, required
        this.inputFieldName = option.inputFieldName;//name of input control, not required，jquery validate
        this.selectType = option.selectType==="multiple"?"multiple":"single";//select type
        this.initValue = option.initValue;//init value，single select is tring，multiple select is array
        this.initInput = option.initInput;//init input，single select is tring，multiple select is array
        this.valueColumn=option.valueColumn?option.valueColumn:option.name;// value column of table（input back to select）
        this.inputColumn=option.inputColumn?option.inputColumn:this.valueColumn;// input column of talbe（input back to input）
        this.showValue=option.showValue === true ? true:false;
        this.editable = option.editable==true?true:false;
        this.readonly = option.readonly==true?true:false;
        this.title = option.title||"选择";//dialog title
        this.clearable = option.clearable===true?true:false;
        this.url = option.url;
        this.initUrl = option.initUrl;
        this.onSetValue = option.onSetValue; 
        var _self = this;
        // set value and input
        this.setValue=function(value, input){
            if(this.selectType == "single") {
                if ($.type(value)=="array") {
                    value = value[0];
                }
            } else {
                if ($.type(value)=="string"){
                    value = [value];
                }
            }
            this._renderValue(value);
            this._renderInput(this._inputText(input, value));
            if (_self.onSetValue) {
                _self.onSetValue(value);
            }
        };
        // render value
        this._renderValue = function(valueData) {
            this.$valueField.empty();
            if ("single" == this.selectType) {
                if(valueData === "" || valueData === undefined || valueData===null) {
                    this.$valueField.val(valueData);
                    return;
                }
                this.$valueField.append("<option value='" + valueData + "' selected='selected'/>");
            } else {
                if(valueData) {
                    for(var i = 0; i < valueData.length; i++) {
                        this.$valueField.append("<option value='" + valueData[i] + "' selected='selected'/>");
                    }
                }
            }
        }
        // render input
        this._renderInput = function(inputData) {
            this.$inputField.val(inputData);
        }
        
        this._inputText = function(input, value) {
            if(_self.selectType == "single") {
                if (value===undefined || input ===undefined || value===null || input===null) {
                    return '';
                }
                return this.showValue ? value+'-'+input : input;
            } else {
                if (value===undefined || input ===undefined || value===null || input===null) {
                    return '';
                }
                if (! this.showValue) return input;
                var inputText = '';
                for (var i in input){
                    inputText += value[i]+'-'+input[i] + ",";
                }
                return inputText;
            }
        }
        
        this.render=function() {
            this.$container.addClass("j-tableSelect input-group");
            this.$container.append("<select style='display:none' class='form-control dtselect' " + (this.selectType == "multiple" ? "multiple='multiple'":"") + " name='" + this.fieldValueName + "'></select>" +
                    "<input type='text' class='form-control'" + (this.editable ? "":"readOnly='true'") +  (this.inputFieldName ? " name='" + this.inputFieldName + "'" : "") + "/>" +
                            "<div class='input-group-append'><button class='btn btn-outline-secondary' type='button'>...</button>" + 
                            ((this.clearable && !this.editable) ? "<button class='btn btn-outline-secondary' type='button'>X</button>":"") + "</div>");
            $btnGroup=this.$container.find(".input-group-append");
            this.$selectBtn = $btnGroup.find(":contains(...)");
            this.$clearBtn = $btnGroup.find(":contains(X)");
            this.$valueField = this.$container.find("select");
            this.$inputField = this.$container.find("input");
            this._renderValue(this.initValue);
            this._renderInput(this._inputText(this.initInput, this.initValue));
            //ajax get init show value.input format {value:value},output format {input:input},single select is string，multiple select is array
            if (this.initValue !== '' && this.initValue !== null && this.initValue !== undefined && (this.initInput === undefined || this.initInput===''||this.initInput===null) && this.initUrl) {
                this._renderInput(this.initValue);
                $.ajax({url: this.initUrl, data:JSON.stringify({value:this.initValue}),
                    type:"POST", 
                    dataType: "json",
                    contentType : "application/json",
                    success: function(data){
                        _self._renderInput(_self._inputText(data.resultData.input,_self.initValue));
                    }
                });
            }
            // input event
            if(this.editable) {
                this.$inputField.change(function(){
                    var inputData;
                    if(_self.selectType == "single") {
                        inputData = _self.$inputField.val();
                    } else {
                        inputData = _self.$inputField.val().split(",");
                    }
                    _self._renderValue(inputData, inputData);
                });
            }
            if (! _self.readonly) {
            // clear button
            this.$clearBtn.click(function(){
                _self.setValue("","");
            });
            // select button
            this.$selectBtn.click(function(){
                $.jDialog({
                    title:_self.title,
                    url:_self.url+"?selectId=_select_"+$.jSequence.next(),
                    // open callback
                    success:function(){
                        var $dialog = this;
                        var $table = $dialog.find(".table");
                        //close after select in single select mode
                        if(_self.selectType == "single") {
                            //has bug
                            // _self.valueData && $table.bootstrapTable('checkBy', {field: _self.valueColumn, values:[_self.initValue]})
                            // select event
                             $table.on("check.bs.table", function($element, row){
                                   _self.setValue(row[_self.valueColumn], row[_self.inputColumn]);
                                   $dialog.modal("hide");
                                 });  
                             
                            //close after click confirm button
                        } else {
                            //has bug
                            //_self.valueData&& $table.bootstrapTable('checkBy', {field: _self.valueColumn, values:_self.initValue})
                            $dialog.find(".j-btn-ok").on("click", function(){
                                var selections = $table.bootstrapTable("getSelections");
                                var valueData = [];
                                var inputData = [];
                                for(var i in selections) {
                                    valueData.push(selections[i][_self.valueColumn]);
                                    inputData.push(selections[i][_self.inputColumn]);
                                }
                                _self.setValue(valueData, inputData);
                               $dialog.modal("hide");
                            });
                        }
                    }
                });
            });
         }
            
        }
        
  };

  $.fn.jTableSelect=function(option) { 
        // create datatableSelect
    if (typeof option !== 'string') {
        option.container = this;
        var tableSelect = new JTableSelect(option);
        tableSelect.render();
        $(this).data("jTableSelect",tableSelect);
      }
 };
 
 // List select control
 var JListSelect = function (option) {
        this.$container = option.container;
        this.initValue = option.initValue;//init value，array
        this.initUrl = option.initUrl;//init show url
        var _self = this;
        this.render=function() {
            this.$container.addClass("j-listSelect input-group");
            this.$container.append("<textarea class='form-control' name='"+this.fieldValueName + "'></textarea>" +
                            "<div class='input-group-append'><button class='btn btn-outline-secondary j-list-select-btn' type='button'>" + J.msg['jListSelect.view'] +"</button>");
            this.$selectBtn = this.$container.find(".j-list-select-btn");
            this.$valueField = this.$container.find("textarea");
            this.$valueField.text(this.initValue);
            // Select Button
            this.$selectBtn.click(function(){
                var value = _self.$valueField.val();
                if (value === undefined || value === null || value === "") {
                    alert(J.msg['jListSelect.inputTip']);
                    return;
                }
                var values;
                if (value.trim().charAt(0)=='[') {
                    values = JSON.parse(value);
                } else {
                    values = value.split(",");
                }
                $.ajax({url: _self.initUrl, data:JSON.stringify({value:values}),
                    type:"POST", 
                    dataType: "json",
                    contentType : "application/json",
                    success: function(data){
                        var input = data.resultData.input;
                        var table = '<table class="table table-striped table-bordered"><thead><tr><th>' + J.msg['jListSelect.value'] + '</th><th>' + + J.msg['jListSelect.info'] + '</th></tr></thead>';
                        for (var i = 0; i < values.length; i++) {
                            table += "<tr><td>" + values[i] + "</td><td>" + input[i] + "</td></tr>";
                        }
                        table += "</table>";
                        var modal = "<div>" + table + "</div>";
                        $.jDialog({title:J.msg["jDialog.view"],element:modal});
                    }
                });
            });
         }
     this.setValue = function() {
         
     }
 }
 $.fn.jListSelect=function(option) { 
        // Create listSelect
    if (typeof option !== 'string') {
        option.container = this;
        var listSelect = new JListSelect(option);
        listSelect.render();
        $(this).data("jListSelect",listSelect);
      }
}; 
 
  $(function () {
      $(document).on("submit", ".j-ajax-form", function(){
          $(this).jAjaxSubmit();
          return false;
          });
  });   
  
// JSON PATH
J.jp = {
            parse:function(exp) {
                var parts = exp.split(/[\.|\[]/);
                var result = [];
                for (var i in parts) {
                    if (parts[i] == "$") {
                        continue;
                    }
                    // Array
                    if (parts[i].indexOf("]")>0) {
                        result.push({type:"array", val:parts[i].substr(0, parts[i].length - 1)});
                    } else {
                        //Object
                        result.push({type:"obj",val:parts[i]});
                    }
                }
                return result;
            },
            value:function(obj, exp, newValue) {
                // Query mode
                if (newValue === undefined) {
                    var result = obj;
                    if (obj === undefined || obj === null) {
                        return result;
                    }
                    var parts = this.parse(exp);
                    for (var i in parts) {
                        var part = parts[i];
                        obj = obj[part.val];
                        if (obj === undefined || obj === null) {
                            return obj;
                        }
                    }
                    return obj;
                // Set value mode
                } else {
                    var parts = this.parse(exp);
                    var parent,parentAttr;
                    var current = obj;
                    for (var i in parts) {
                        if (!current) {
                            current = parts[i].type=='array' ? []:{};
                            if (i == 0){obj=current}
                            parent && (parent[parentAttr]=current);
                        }
                        if (i == parts.length - 1) {
                            current[parts[i].val] = newValue;
                            break;
                        }
                        
                        parent = current;
                        parentAttr = parts[i].val;
                        current = current[parts[i].val];
                    }
                    return obj;
                }
            }
                
        }

function timestampToTime(timestampMs) {
    let date = new Date(timestampMs);
    let Y = date.getFullYear() + '-';
    let M = (date.getMonth()+1 < 10 ? '0'+(date.getMonth()+1) : date.getMonth()+1) + '-';
    let D = date.getDate() + ' ';
    let h = date.getHours() + ':';
    let m = date.getMinutes() + ':';
    let s = date.getSeconds();
    return Y+M+D+h+m+s;
}
// Money format
J.formatMoney = function(money, sysmbol = '', places = 2) {
    const zero = `${sysmbol}0.00`;
    if (isNaN(money) || money === '' || money === undefined || money===null) return zero;
         money = `${money}`;
         let left=money.split('.')[0]; 
         let right=money.split('.')[1]; 
         right = right ? (right.length >= places ? '.' + right.substr(0, places) : '.' + right + '0'.repeat(places - right.length)) : ('.' + '0'.repeat(places));
         var temp = left.split('').reverse().join('').match(/(\d{1,3})/g); 
         return (Number(money) < 0 ? '-' : '') + sysmbol + temp.join(',').split('').reverse().join('') + right;
 };
 J.formatTime = function(timestampMs) {
    if(timestampMs === undefined || timestampMs === "") {
        return "";
    }
     var check = function(m) {
         return m<10 ? '0' + m : m
     };
     var time = new Date(timestampMs);
     var y = time.getFullYear();
     var m = time.getMonth()+1;
     var d = time.getDate();
     var h = time.getHours();
     var mm = time.getMinutes();
     var s = time.getSeconds();
     return y+'-'+check(m)+'-'+check(d)+' '+check(h)+':'+check(mm)+':'+check(s);
 };
J.formatTimeYmd = function(timestampMs) {
    if(timestampMs === undefined || timestampMs === "") {
        return "";
    }
    var check = function(m) {
        return m<10 ? '0' + m : m
    };
    var time = new Date(timestampMs);
    var y = time.getFullYear();
    var m = time.getMonth()+1;
    var d = time.getDate();
    return y+'-'+check(m)+'-'+check(d);
};
J.formatTimeYm = function(timestampMs) {
    if(timestampMs === undefined || timestampMs === "") {
        return "";
    }
    var check = function(m) {
        return m<10 ? '0' + m : m
    };
    var time = new Date(timestampMs);
    var y = time.getFullYear();
    var m = time.getMonth()+1;
    return y+'-'+check(m);
};
//yyyy-mm-dd hh:mm:ss
J.formatTimestampOrStrTime = function(timesMs) {
    if(timesMs === undefined || timesMs === "") {
        return "";
    }
    var time = new Date(timesMs);
    if(time == 'Invalid Date' ){ //yyyymmddhhmmss convert to yyyy-mm-dd hh:mm:ss
        time = timesMs.replace(/^(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})$/, "$1-$2-$3 $4:$5:$6")
        return time;
    }
    var check = function(m) {
        return m<10 ? '0' + m : m
    };
    var y = time.getFullYear();
    var m = time.getMonth()+1;
    var d = time.getDate();
    var h = time.getHours();
    var mm = time.getMinutes();
    var s = time.getSeconds();
    return y+'-'+check(m)+'-'+check(d)+' '+check(h)+':'+check(mm)+':'+check(s);
};

/**
 * 打开窗口
 */
J.openWindow=function(url, name, specs, replace) {
    if (self != top) {
        window.location.href=url;
    } else {
        window.open(url, name, specs, replace);
    }
}

/**
 * 动态计算js，打印异常信息.
 */
J.eval = function(js, ctxData) {
    try {
        eval(js);
    } catch (err) {
        console.dir(err);
        console.log("eval异常, js:" + js);
        if (ctxData) {
            console.log("ctxData:" + JSON.stringify(ctxData));
        };
        throw err;
    }
}




