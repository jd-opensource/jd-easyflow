package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class UnlockProcessInstanceReq implements Serializable {

    String processType;
    
    String bizNo;
    
    String lockRequestId;

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

    public String getLockRequestId() {
        return lockRequestId;
    }

    public void setLockRequestId(String lockRequestId) {
        this.lockRequestId = lockRequestId;
    }

    @Override
    public String toString() {
        return "UnlockProcessInstanceReq [processType=" + processType + ", bizNo=" + bizNo + ", lockRequestId="
                + lockRequestId + "]";
    }
    
    
}
