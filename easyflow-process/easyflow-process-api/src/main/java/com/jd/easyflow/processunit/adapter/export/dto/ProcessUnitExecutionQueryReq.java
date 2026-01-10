package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitExecutionQueryReq implements Serializable {
    
    private String executionNo;
    
    private String instanceNo;

    private String bizNo;
    
    private String processUnitCode;
    
    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getProcessUnitCode() {
        return processUnitCode;
    }

    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    @Override
    public String toString() {
        return "ProcessUnitExecutionQueryReq [executionNo=" + executionNo + ", instanceNo=" + instanceNo + ", bizNo="
                + bizNo + ", processUnitCode=" + processUnitCode + "]";
    }


}
