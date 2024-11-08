package com.jd.easyflow.common.util;

/**
 *
 * @author liyuliang5
 */
public enum CommonErrorCode {
    E0000000("Success"),
    E0000001("Fail"),
    E0000002("Validate not pass"),
    E0000003("Record exists"),
    E0000004("Record not exists"),
    E0000005("Field is emplty"),
    E0000006("Not login"),
    E0000007("RPC invoke exception"),
    E0000008("Field validate unpass"),
    E0000010("Enum validate exception"),
    E0000009("Cache service exception"),
    E0000012("No auth"),

    E9999999("System exception");

    private String desc;
    private String showMessage;

    CommonErrorCode(String desc) {
        this.desc = desc;
        this.showMessage = desc;
    }

    CommonErrorCode(String desc, String showMessage) {
        this.desc = desc;
        this.showMessage = showMessage;
    }

    public static CommonErrorCode getEnum(String code) {
        for (CommonErrorCode a : CommonErrorCode.values()) {
            if (a.name().substring(1).equals(code)) {
                return a;
            }
        }
        throw new IllegalArgumentException("No enum code '" + code + "'. " + CommonErrorCode.class);
    }

    public String getCode() {
        return name().substring(1);
    }

    public String getDesc() {
        return desc;
    }

    public String getShowMessage() {
        return showMessage == null ? desc : showMessage;
    }

}
