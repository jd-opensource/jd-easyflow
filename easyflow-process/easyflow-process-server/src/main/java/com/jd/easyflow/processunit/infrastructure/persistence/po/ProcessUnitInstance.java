package com.jd.easyflow.processunit.infrastructure.persistence.po;

import java.util.Date;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitInstance {
    private Long id;

    private String instanceNo;

    private String bizNo;

    private String parentNo;

    private String processUnitCode;

    private String productCode;

    private String result;

    private Boolean autoRunFlag;

    private Integer autoRunTimes;

    private Date nextAutoRunTime;
    
    private String requestContent;
    
    private String responseContent;
    
    private String vars;

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

    public Boolean getAutoRunFlag() {
        return autoRunFlag;
    }

    public void setAutoRunFlag(Boolean autoRunFlag) {
        this.autoRunFlag = autoRunFlag;
    }

    public Integer getAutoRunTimes() {
        return autoRunTimes;
    }

    public void setAutoRunTimes(Integer autoRunTimes) {
        this.autoRunTimes = autoRunTimes;
    }

    public Date getNextAutoRunTime() {
        return nextAutoRunTime;
    }

    public void setNextAutoRunTime(Date nextAutoRunTime) {
        this.nextAutoRunTime = nextAutoRunTime;
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
    
    public String getVars() {
        return vars;
    }

    public void setVars(String vars) {
        this.vars = vars;
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
        return "ProcessUnitInstance [id=" + id + ", instanceNo=" + instanceNo + ", bizNo=" + bizNo + ", parentNo="
                + parentNo + ", processUnitCode=" + processUnitCode + ", productCode=" + productCode + ", result="
                + result + ", autoRunFlag=" + autoRunFlag + ", autoRunTimes=" + autoRunTimes + ", nextAutoRunTime="
                + nextAutoRunTime + ", requestContent=" + requestContent + ", responseContent=" + responseContent
                + ",vars=" + vars + ", extData=" + extData + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate
                + ", deleted=" + deleted + "]";
    }
    
    
}