/**
 * Created by fenghao1 on 2016/6/18.
 */
$(function(){
    function checkId(id) {
        if(typeof id !== 'string') return {code:false,msg:'非字符串'};
        var province = {11:"北京",12:"天津",13:"河北",14:"山西",15:"内蒙古",21:"辽宁",22:"吉林",23:"黑龙江 ",31:"上海",32:"江苏",33:"浙江",34:"安徽",35:"福建",36:"江西",37:"山东",41:"河南",42:"湖北 ",43:"湖南",44:"广东",45:"广西",46:"海南",50:"重庆",51:"四川",52:"贵州",53:"云南",54:"西藏 ",61:"陕西",62:"甘肃",63:"青海",64:"宁夏",65:"新疆",71:"台湾",81:"香港",82:"澳门",91:"国外"};
        var birthday = id.substr(6, 4) + '/' + Number(id.substr(10, 2)) + '/' + Number(id.substr(12, 2));
        var d = new Date(birthday);
        var newBirthday = d.getFullYear() + '/' + Number(d.getMonth() + 1) + '/' + Number(d.getDate());
        var currentTime = new Date().getTime();
        var time = d.getTime();
        var arrInt = [7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2];
        var arrCh = ['1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2'];
        var sum = 0, i, residue;

        if(!/^\d{17}(\d|x)$/i.test(id)) return {code:false,msg:'非法身份证'};
        if(province[id.substr(0,2)] === undefined) return {code:false,msg:'非法地区'};
        if(time >= currentTime || birthday !== newBirthday) return {code:false,msg:'非法生日'};
        for(i=0; i<17; i++) {
            sum += id.substr(i, 1) * arrInt[i];
        }
        residue = arrCh[sum % 11];
        if (residue !== id.substr(17, 1)) return {code:false,msg:'非法身份证哦'};

        return {code:true, msg:'身份证正确'};
    }

    $.validator.addMethod("valid16Code", function(value, element) {
        return this.optional(element) || /^[A-Za-z0-9\-_\u4E00-\u9FA5]{1,16}$/.test(value);
    }, "只能包括中文、英文、数字、横线和下划线，最大长度16个字符");
    $.validator.addMethod("valid32Code", function(value, element) {
        return this.optional(element) || /^[A-Za-z0-9\-_\u4E00-\u9FA5]{1,32}$/.test(value);
    }, "只能包括中文、英文、数字、横线和下划线，最大长度32个字符");
    $.validator.addMethod("valid64Desc", function(value, element) {
        return this.optional(element) || /^[A-Za-z0-9\-_\u4E00-\u9FA5]{1,64}$/.test(value);
    }, "只能包括中文、英文、数字、横线和下划线，最大长度64个字符");
    $.validator.addMethod("valid128Desc", function(value, element) {
        return this.optional(element) || /^[A-Za-z0-9\-_\u4E00-\u9FA5]{1,128}$/.test(value);
    }, "只能包括中文、英文、数字、横线和下划线，最大长度128个字符");
    $.validator.addMethod("valid", function(value, element, params) {
        var reg = new RegExp('^[A-Za-z0-9\-_\u4E00-\u9FA5]{1,' + params + '}$');
        return this.optional(element) || reg.test(value);
    }, "只能包括中文、英文、数字、横线和下划线，最大长度 {0} 个字符");
    $.validator.addMethod("minVal", function(value, element, params) {
        return this.optional(element) || value > params;
    }, "请输入大于 {0} 的数值");
    $.validator.addMethod("validPhone", function(value, element) {
        return this.optional(element) || /^1\d{10}$/.test(value);
    }, "请输入合法的手机号码");
    $.validator.addMethod("validId", function(value, element) {
        var result = checkId(value);
        return this.optional(element) || result.code;
    }, "请输入合法的身份证号码");
    $.validator.addMethod("notEqualsTo", function(value, element, params) {
        var otherVal = $(params[0]).val();
        return this.optional(element) || otherVal != value;
    }, "不能和{1}相同");
    $.validator.addMethod("rangeRate", function(value, element, params) {
        var min = +($(params[0]).data('minRate'));
        var max = +($(params[0]).data('maxRate'));
        return this.optional(element) || value >= min && value <= max;
    }, "填写的利率不在范围内");
});
