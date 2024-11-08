package com.jd.easyflow.process.domain.model.entity;

import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskEventEntity {

    private Long id;

    private String eventNo;

    private String taskNo;

    private String eventType;

    private String eventUser;

    private Date eventTime;

    private String eventBizResult;

    private String eventBizData;
    
    private String instanceBizStatus;

    private String instanceBizData;    

    private String productCode;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEventNo() {
        return eventNo;
    }

    public void setEventNo(String eventNo) {
        this.eventNo = eventNo;
    }

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public String getEventUser() {
        return eventUser;
    }

    public void setEventUser(String eventUser) {
        this.eventUser = eventUser;
    }

    public Date getEventTime() {
        return eventTime;
    }

    public void setEventTime(Date eventTime) {
        this.eventTime = eventTime;
    }

    public String getEventBizResult() {
        return eventBizResult;
    }

    public void setEventBizResult(String eventBizResult) {
        this.eventBizResult = eventBizResult;
    }

    public String getEventBizData() {
        return eventBizData;
    }

    public void setEventBizData(String eventBizData) {
        this.eventBizData = eventBizData;
    }

    public String getInstanceBizStatus() {
        return instanceBizStatus;
    }

    public void setInstanceBizStatus(String instanceBizStatus) {
        this.instanceBizStatus = instanceBizStatus;
    }

    public String getInstanceBizData() {
        return instanceBizData;
    }

    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
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
        return "ProcessTaskEventEntity [id=" + id + ", eventNo=" + eventNo + ", taskNo=" + taskNo + ", eventType="
                + eventType + ", eventUser=" + eventUser + ", eventTime=" + eventTime + ", eventBizResult="
                + eventBizResult + ", eventBizData=" + eventBizData + ", instanceBizStatus=" + instanceBizStatus
                + ", instanceBizData=" + instanceBizData + ", productCode=" + productCode + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
}
