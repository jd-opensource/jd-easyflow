package com.jd.easyflow.process.client.task;

/**
 * @author liyuliang5
 *
 */
public enum TaskErrorCode {

    PTC_0101("Call biz client execption");

    private String desc;

    TaskErrorCode(String desc) {
        this.desc = desc;
    }

    public String getDesc() {
        return desc;
    }

}
