package com.jd.easyflow.admin.process.adapter.page.util;

/**
 * @author liyuliang5
 *
 */
public enum ClientErrorCode {

    PTC_0101("Callback service error");

    private String desc;

    ClientErrorCode(String desc) {
        this.desc = desc;
    }

    public String getDesc() {
        return desc;
    }

}
