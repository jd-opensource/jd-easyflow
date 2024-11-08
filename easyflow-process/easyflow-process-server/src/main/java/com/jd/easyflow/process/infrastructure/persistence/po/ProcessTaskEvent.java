package com.jd.easyflow.process.infrastructure.persistence.po;

import java.util.Date;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskEvent {
    
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

    private boolean deleted;

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

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
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

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", eventNo=").append(eventNo);
        sb.append(", taskNo=").append(taskNo);
        sb.append(", eventType=").append(eventType);
        sb.append(", eventUser=").append(eventUser);
        sb.append(", eventTime=").append(eventTime);
        sb.append(", eventBizResult=").append(eventBizResult);
        sb.append(", eventBizData=").append(eventBizData);
        sb.append(", instanceBizStatus=").append(instanceBizStatus);
        sb.append(", instanceBizData=").append(instanceBizData);        
        sb.append(", productCode=").append(productCode);
        sb.append(", extData=").append(extData);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append("]");
        return sb.toString();
    }
}