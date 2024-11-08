J.FsmInput=function(option) {
    this.$container = option.container;
    this.$innerContainer = $("<div></div>").appendTo(this.$container);    
    this.name = option.name;
    this.jTableSelect = new JTableSelect({
        container:this.$innerContainer,
        name:this.name + "_val",
        inputFieldName:this.name,
        valueColumn:"defId",
        editable:true,
        url:$.getBaseUrl()+"/easyflow/processDefinition/ajax/processSelectFrame"
    });
    this.render=function() {
        var _self = this;
        this.jTableSelect.render();
        this.$innerContainer.find(".input-group-append").append('<button class="btn btn-outline-secondary j-fsm-view" type="button">' + J.msg['common.view'] + '</button>');
        this.$innerContainer.find(".j-fsm-view").click(function() {
            var defId = _self.jTableSelect.$inputField.val();
            J.openWindow($.getBaseUrl() + "/easyflow/processDefinition/detail?defId=" + defId + "&latest=true","_blank", "", "");
        });
    }
};
$.fn.jFsmInput=function(option) {
 if (!option) {
     option = {};
 }
if (typeof option !== 'string') {
    option.container = this;
    var fsmInput = new J.FsmInput(option);
    option.container.data("jFsmInput",fsmInput);
    fsmInput.render();
  } else {
      if (option == "setValue") {
          this.data("jFsmInput").setValue();
      }
  }
};

J.FlowInput=function(option) {
    this.$container = option.container;
    this.$innerContainer = $("<div></div>").appendTo(this.$container);    
    this.name = option.name;
    this.jTableSelect = new JTableSelect({
        container:this.$innerContainer,
        name:this.name + "_val",
        inputFieldName:this.name,
        valueColumn:"defId",
        editable:true,
        url:$.getBaseUrl()+"/easyflow/processDefinition/ajax/processSelectFrame"
    });
    this.render=function() {
        var _self = this;
        this.jTableSelect.render();
        this.$innerContainer.find(".input-group-append").append('<button class="btn btn-outline-secondary j-flow-view" type="button">' + J.msg['common.view'] + '</button>');
        this.$innerContainer.find(".j-flow-view").click(function() {
            var defId = _self.jTableSelect.$inputField.val();
            J.openWindow($.getBaseUrl() + "/easyflow/processDefinition/detail?defId=" + defId + "&latest=true","", "", "");
        });
    }
};
$.fn.jFlowInput=function(option) {
 if (!option) {
     option = {};
 }
if (typeof option !== 'string') {
    option.container = this;
    var flowInput = new J.FlowInput(option);
    option.container.data("jFlowInput",flowInput);
    flowInput.render();
  } else {
      if (option == "setValue") {
          this.data("jFlowInput").setValue();
      }
  }
};

J.MaskInput=function(option) {
    this.$input = option.$input;
    var plaintextFunc = option.plaintextFunc;
    this.render=function() {
        var $input = this.$input;
        var $icon = $('<i class="j-mask-input fa fa-eye-slash"></i>').insertAfter(this.$input);
        $icon.click(function(){
            if (!$input.data('j-masktext')) {
                $input.data('j-masktext', $input.val());
            }
            if (!$input.data('j-plaintext-call')) {
                  plaintextFunc.call(this, function(data){
                      $input.data('j-plaintext', data.plaintext);
                      $icon.removeClass('fa-eye-slash').addClass('fa-eye');
                      $input.val($input.data('j-plaintext'));
                      $input.data('j-plaintext-call', 'true');
                  });
            } else {
                if ($icon.hasClass('fa-eye-slash')) {
                    $icon.removeClass('fa-eye-slash').addClass('fa-eye');
                    $input.val($input.data('j-plaintext'));
                } else {
                    $icon.removeClass('fa-eye').addClass('fa-eye-slash');
                    $input.val($input.data('j-masktext'));
                }
            }
            
        });
        
    }
};

$.fn.jMaskInput=function(option) {
 if (!option) {
     option = {};
 }
if (typeof option !== 'string') {
    option.$input = this;
    var maskInput = new J.MaskInput(option);
    maskInput.render();
  } else {
        // NOOP
  }
};