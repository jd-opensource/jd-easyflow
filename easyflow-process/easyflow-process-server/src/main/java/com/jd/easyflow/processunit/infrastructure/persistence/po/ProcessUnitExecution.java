package com.jd.easyflow.processunit.infrastructure.persistence.po;

import java.util.Date;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitExecution {
    private Long id;

    private String executionNo;

    private String requestNo;

    private String parentNo;

    private String processUnitCode;

    private String bizNo;

    private String instanceNo;

    private String productCode;

    private String result;

    private Date requestTime;

    private Date responseTime;

    private Integer elaspeTime;

    private String execType;
    
    private String requestContent;
    
    private String responseContent;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    private boolean deleted;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

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

    public String getProcessUnitCode() {
        return processUnitCode;
    }

    public void setProcessUnitCode(String processUnitCode) {
        this.processUnitCode = processUnitCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
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

    public Integer getElaspeTime() {
        return elaspeTime;
    }

    public void setElaspeTime(Integer elaspeTime) {
        this.elaspeTime = elaspeTime;
    }

    public String getExecType() {
        return execType;
    }

    public void setExecType(String execType) {
        this.execType = execType;
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

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    @Override
    public String toString() {
        return "ProcessUnitExecution [id=" + id + ", executionNo=" + executionNo + ", requestNo=" + requestNo
                + ", parentNo=" + parentNo + ", processUnitCode=" + processUnitCode + ", bizNo=" + bizNo + ", instanceNo=" + instanceNo
                + ", productCode=" + productCode + ", result=" + result + ", requestTime=" + requestTime
                + ", responseTime=" + responseTime + ", elaspeTime=" + elaspeTime + ", execType=" + execType
                + ", requestContent=" + requestContent + ", responseContent=" + responseContent + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + ", deleted=" + deleted + "]";
    }
    
    

}