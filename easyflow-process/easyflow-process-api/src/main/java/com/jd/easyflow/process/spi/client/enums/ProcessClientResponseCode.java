package com.jd.easyflow.process.spi.client.enums;

/**
 * @author liyuliang5
 *
 */
public enum ProcessClientResponseCode {

    SUCCESS("0000000","SUCCESS"),
    PROCESS_INSTANCE_EXISTS("0100001","Process instance exists");

    private String code;

    private String desc;

    ProcessClientResponseCode(String code, String desc) {
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
