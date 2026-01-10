package com.jd.easyflow.processunit.client.bean;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitExecution {

    private String executionNo;
    
    private String requestNo;
    
    private String parentNo;
    
    private String instanceNo;
    
    private String processUnitCode;
    
    private String productCode;
    
    private String result;
 
    private String requestContent;

    private String responseContent;
    
    private String responseCode;
    
    private Date requestTime;
    
    private Date responseTime;
    
    private long elaspeTime;

    private String extData;
    
    private Date createdDate;
    
    private Date modifiedDate;

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getRequestNo() {
        return requestNo;
    }

    public void setRequestNo(String requestNo) {
        this.requestNo = requestNo;
    }

    public String getParentNo() {
        return parentNo;
    }

    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
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

    public String getResponseCode() {
        return responseCode;
    }

    public void setResponseCode(String responseCode) {
        this.responseCode = responseCode;
    }

    public Date getRequestTime() {
        return requestTime;
    }

    public void setRequestTime(Date requestTime) {
        this.requestTime = requestTime;
    }

    public Date getResponseTime() {
        return responseTime;
    }

    public void setResponseTime(Date responseTime) {
        this.responseTime = responseTime;
    }

    public long getElaspeTime() {
        return elaspeTime;
    }

    public void setElaspeTime(long elaspeTime) {
        this.elaspeTime = elaspeTime;
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

    @Override
    public String toString() {
        return "ProcessUnitExecution [executionNo=" + executionNo + ", requestNo=" + requestNo + ", parentNo="
                + parentNo + ", instanceNo=" + instanceNo + ", processUnitCode=" + processUnitCode + ", productCode="
                + productCode + ", result=" + result + ", requestContent=" + requestContent + ", responseContent="
                + responseContent + ", responseCode=" + responseCode + ", requestTime=" + requestTime
                + ", responseTime=" + responseTime + ", elaspeTime=" + elaspeTime + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
}
