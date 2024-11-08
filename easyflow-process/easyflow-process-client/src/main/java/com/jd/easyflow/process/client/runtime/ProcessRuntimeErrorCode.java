package com.jd.easyflow.process.client.runtime;

/**
 * 
 * @author liyuliang5
 *
 */
public enum ProcessRuntimeErrorCode {

    PR_0101("Start node close, can not execute"),
    PR_0102("Not start node or open node, can not execute"),
    PR_0103("Process instance is closed, can not execute"),
    PR_0104("Process instance is canncelled, can not execute");

    private String desc;

    ProcessRuntimeErrorCode(String desc) {
        this.desc = desc;
    }

    public String getDesc() {
        return desc;
    }

}
