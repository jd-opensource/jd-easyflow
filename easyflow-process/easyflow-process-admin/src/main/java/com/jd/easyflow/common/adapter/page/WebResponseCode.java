package com.jd.easyflow.common.adapter.page;

/**
 * @author liyuliang5
 *
 */
public enum WebResponseCode {


    SUCCESS("0000000","Success"),

    FAIL("0000001","Fail"),

    DATA_EMPTY("0000004","Data empty"),

    FIELD_EMPTY("0000005","Field empty"),

    INVALID("0000002","Validate not pass"),

    EXCEPTION("9999999","System exception"),
            ;

    private String code;

    private String desc;

    WebResponseCode(String code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    public String getCode() {
        return code;
    }

    public String getDesc() {
        return desc;
    }
}
