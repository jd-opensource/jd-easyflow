package com.jd.easyflow.processunit.domain.model.enums;

/**
 * 
 * @author liyuliang5
 *
 */
public enum ProcessUnitErrorCodeEnum {

    PU_0101("Process unit server stopped");

    private String desc;

    ProcessUnitErrorCodeEnum(String desc) {
        this.desc = desc;
    }

    public String getDesc() {
        return desc;
    }

}
