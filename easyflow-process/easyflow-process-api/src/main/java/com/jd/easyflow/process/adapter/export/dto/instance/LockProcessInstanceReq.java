package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class LockProcessInstanceReq implements Serializable {

    String processType;
    
    String bizNo;

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    @Override
    public String toString() {
        return "LockProcessInstanceReq [processType=" + processType + ", bizNo=" + bizNo + "]";
    }
    
    
}
