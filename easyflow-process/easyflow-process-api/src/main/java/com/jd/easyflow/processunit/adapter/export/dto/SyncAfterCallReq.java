package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class SyncAfterCallReq implements Serializable {

    private String executionNo;

    private String result;

    private String responseContent;

    private String instanceNo;

    private String bizNo;

    private String unitCode;

    private Map<String, String> contextData;
    
    private Map<String, String> variables;
    
    private Boolean autoRunFlag;
    
    private Date nextAutoRunTime;

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getResponseContent() {
        return responseContent;
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getUnitCode() {
        return unitCode;
    }

    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }

    public Map<String, String> getContextData() {
        return contextData;
    }

    public void setContextData(Map<String, String> contextData) {
        this.contextData = contextData;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }
    

    public Boolean getAutoRunFlag() {
        return autoRunFlag;
    }

    public void setAutoRunFlag(Boolean autoRunFlag) {
        this.autoRunFlag = autoRunFlag;
    }

    public Date getNextAutoRunTime() {
        return nextAutoRunTime;
    }

    public void setNextAutoRunTime(Date nextAutoRunTime) {
        this.nextAutoRunTime = nextAutoRunTime;
    }

    @Override
    public String toString() {
        return "SyncAfterCallReq [executionNo=" + executionNo + ", result=" + result + ", responseContent="
                + responseContent + ", instanceNo=" + instanceNo + ", bizNo=" + bizNo + ", unitCode=" + unitCode
                + ", contextData=" + contextData + ", variables=" + variables  + ", autoRunFlag=" + autoRunFlag + ", nextAutoRunTime=" + nextAutoRunTime + "]";
    }
    
    
}
