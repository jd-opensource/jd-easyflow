package com.jd.easyflow.process.infrastructure.persistence.po;

import java.util.Date;

/**
 * @author liyuliang5
 * 
 */
public class ProcessInstance {
    
    private Long id;

    private String instanceNo;
    
    private String instanceName;

    private String processType;

    private String bizNo;

    private String productCode;
    
    private String keyField;

    private String keyField2;

    private String creator;

    private String processDefId;

    private Date startTime;

    private Date endTime;

    private String status;

    private String parentInstanceNo;
    
    private String parentNodeInstanceNo;
    
    private String currentNodeIds;
    
    private String bizStatus;
    
    private String bizData;

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

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getCreator() {
        return creator;
    }

    public void setCreator(String creator) {
        this.creator = creator;
    }

    public String getProcessDefId() {
        return processDefId;
    }

    public void setProcessDefId(String processDefId) {
        this.processDefId = processDefId;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getParentNodeInstanceNo() {
        return parentNodeInstanceNo;
    }

    public void setParentNodeInstanceNo(String parentNodeInstanceNo) {
        this.parentNodeInstanceNo = parentNodeInstanceNo;
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
    

    public String getParentInstanceNo() {
        return parentInstanceNo;
    }

    public void setParentInstanceNo(String parentInstanceNo) {
        this.parentInstanceNo = parentInstanceNo;
    }

    public String getCurrentNodeIds() {
        return currentNodeIds;
    }

    public void setCurrentNodeIds(String currentNodeIds) {
        this.currentNodeIds = currentNodeIds;
    }
    
    public String getInstanceName() {
        return instanceName;
    }

    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }
    
    public String getBizStatus() {
        return bizStatus;
    }

    public void setBizStatus(String bizStatus) {
        this.bizStatus = bizStatus;
    }

    public String getBizData() {
        return bizData;
    }

    public void setBizData(String bizData) {
        this.bizData = bizData;
    }
    
    public String getKeyField() {
        return keyField;
    }

    public void setKeyField(String keyField) {
        this.keyField = keyField;
    }

    public String getKeyField2() {
        return keyField2;
    }

    public void setKeyField2(String keyField2) {
        this.keyField2 = keyField2;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", instanceNo=").append(instanceNo);
        sb.append(", instanceName=").append(instanceName);
        sb.append(", processType=").append(processType);
        sb.append(", bizNo=").append(bizNo);
        sb.append(", productCode=").append(productCode);
        sb.append(", keyField=").append(keyField);
        sb.append(", keyField2=").append(keyField2);
        sb.append(", creator=").append(creator);
        sb.append(", processDefId=").append(processDefId);
        sb.append(", startTime=").append(startTime);
        sb.append(", endTime=").append(endTime);
        sb.append(", status=").append(status);
        sb.append(", parentInstanceNo=").append(parentInstanceNo);
        sb.append(", parentNodeInstanceNo=").append(parentNodeInstanceNo);
        sb.append(", currentNodeIds=").append(currentNodeIds);
        sb.append(", bizStatus=").append(bizStatus);
        sb.append(", bizData=").append(bizData);
        sb.append(", vars=").append(vars);
        sb.append(", extData=").append(extData);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append("]");
        return sb.toString();
    }
}