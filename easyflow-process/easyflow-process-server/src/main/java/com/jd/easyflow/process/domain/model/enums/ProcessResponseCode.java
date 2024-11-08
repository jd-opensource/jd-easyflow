package com.jd.easyflow.process.domain.model.enums;

import com.jd.easyflow.spring.MessageUtil;

/**
 * @author liyuliang5
 *
 */
public enum ProcessResponseCode {

    PROCESS_INSTANCE_EXISTS("0100001",MessageUtil.getMessage("easyflow.process.server.tip.processInstanceExists"));

    private String code;

    private String desc;

    ProcessResponseCode(String code, String desc) {
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

