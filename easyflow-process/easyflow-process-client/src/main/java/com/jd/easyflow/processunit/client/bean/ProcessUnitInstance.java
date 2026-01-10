package com.jd.easyflow.processunit.client.bean;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitInstance {

    private String instanceNo;

    private String bizNo;
    
    private String parentNo;
    
    private String processUnitCode;
    
    private String productCode;

    private String result;

    private String requestContent;

    private String responseContent;
    
    private String vars;
        
    private String extData;
    
    private Date createdDate;
    
    private Date modifiedDate;

    private boolean autoRunFlag;

    private int asyncRunTimes;
    private Date nextAsyncRunTime;
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
    public String getParentNo() {
        return parentNo;
    }
    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
    }
    public String getProcessUnitCode() {
        return processUnitCode;
    }
    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }
    public String getProductCode() {
        return productCode;
    }
    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }
    public String getResult() {
        return result;
    }
    public void setResult(String result) {
        this.result = result;
    }
    public String getRequestContent() {
        return requestContent;
    }
    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }
    public String getResponseContent() {
        return responseContent;
    }
    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }
    public String getExtData() {
        return extData;
    }
    public void setExtData(String extData) {
        this.extData = extData;
    }
    public Date getCreatedDate() {
        return createdDate;
    }
    public void setCreatedDate(Date createdDate) {
        this.createdDate = createdDate;
    }
    public Date getModifiedDate() {
        return modifiedDate;
    }
    public void setModifiedDate(Date modifiedDate) {
        this.modifiedDate = modifiedDate;
    }
    public boolean isAutoRunFlag() {
        return autoRunFlag;
    }
    public void setAutoRunFlag(boolean autoRunFlag) {
        this.autoRunFlag = autoRunFlag;
    }
    public int getAsyncRunTimes() {
        return asyncRunTimes;
    }
    public void setAsyncRunTimes(int asyncRunTimes) {
        this.asyncRunTimes = asyncRunTimes;
    }
    public Date getNextAsyncRunTime() {
        return nextAsyncRunTime;
    }
    public void setNextAsyncRunTime(Date nextAsyncRunTime) {
        this.nextAsyncRunTime = nextAsyncRunTime;
    }
    public String getVars() {
        return vars;
    }
    public void setVars(String vars) {
        this.vars = vars;
    }
    @Override
    public String toString() {
        return "ProcessUnitInstance [instanceNo=" + instanceNo + ", bizNo=" + bizNo + ", parentNo=" + parentNo
                + ", processUnitCode=" + processUnitCode + ", productCode=" + productCode + ", result=" + result
                + ", requestContent=" + requestContent + ", responseContent=" + responseContent +",vars=" + vars + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + ", autoRunFlag=" + autoRunFlag
                + ", asyncRunTimes=" + asyncRunTimes + ", nextAsyncRunTime=" + nextAsyncRunTime + "]";
    }
    
    
}
