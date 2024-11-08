package com.jd.easyflow.process.adapter.export.enums;

/**
 * @author liyuliang5
 *
 */
public enum ProcessExportResponseCode {

    PROCESS_INSTANCE_EXISTS("0100001","Process instance exists");

    private String code;

    private String desc;

    ProcessExportResponseCode(String code, String desc) {
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

