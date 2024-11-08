package com.jd.easyflow.process.domain.model.vo;

/**
 * @author liyuliang5
 *
 */
public class QueryTaskEventReqVO {

    private Long id;

    private String eventNo;

    private String taskNo;

    private String eventType;

    private String eventUser;

    private String eventTimeStart;
    
    private String eventTimeEnd;

    private String eventBizResult;

    private Object eventBizData;

    private String productCode;

    private Object extData;

    private String createdDateStart;
    
    private String createdDateEnd;

    private String modifiedDateStart;
    
    private String modifiedDateEnd;

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

    public String getEventTimeStart() {
        return eventTimeStart;
    }

    public void setEventTimeStart(String eventTimeStart) {
        this.eventTimeStart = eventTimeStart;
    }

    public String getEventTimeEnd() {
        return eventTimeEnd;
    }

    public void setEventTimeEnd(String eventTimeEnd) {
        this.eventTimeEnd = eventTimeEnd;
    }

    public String getEventBizResult() {
        return eventBizResult;
    }

    public void setEventBizResult(String eventBizResult) {
        this.eventBizResult = eventBizResult;
    }

    public Object getEventBizData() {
        return eventBizData;
    }

    public void setEventBizData(Object eventBizData) {
        this.eventBizData = eventBizData;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public Object getExtData() {
        return extData;
    }

    public void setExtData(Object extData) {
        this.extData = extData;
    }

    public String getCreatedDateStart() {
        return createdDateStart;
    }

    public void setCreatedDateStart(String createdDateStart) {
        this.createdDateStart = createdDateStart;
    }

    public String getCreatedDateEnd() {
        return createdDateEnd;
    }

    public void setCreatedDateEnd(String createdDateEnd) {
        this.createdDateEnd = createdDateEnd;
    }

    public String getModifiedDateStart() {
        return modifiedDateStart;
    }

    public void setModifiedDateStart(String modifiedDateStart) {
        this.modifiedDateStart = modifiedDateStart;
    }

    public String getModifiedDateEnd() {
        return modifiedDateEnd;
    }

    public void setModifiedDateEnd(String modifiedDateEnd) {
        this.modifiedDateEnd = modifiedDateEnd;
    }

    @Override
    public String toString() {
        return "QueryTaskEventReqVO [id=" + id + ", eventNo=" + eventNo + ", taskNo=" + taskNo + ", eventType="
                + eventType + ", eventUser=" + eventUser + ", eventTimeStart=" + eventTimeStart + ", eventTimeEnd="
                + eventTimeEnd + ", eventBizResult=" + eventBizResult + ", eventBizData=" + eventBizData
                + ", productCode=" + productCode + ", extData=" + extData + ", createdDateStart=" + createdDateStart
                + ", createdDateEnd=" + createdDateEnd + ", modifiedDateStart=" + modifiedDateStart
                + ", modifiedDateEnd=" + modifiedDateEnd + "]";
    }
    
    
}
