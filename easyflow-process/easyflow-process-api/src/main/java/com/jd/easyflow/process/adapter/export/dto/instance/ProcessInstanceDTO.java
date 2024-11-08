package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessInstanceDTO implements Serializable {


    private String instanceNo;

    private String instanceName;

    private String processType;

    private String bizNo;

    private String productCode;

    private String creator;

    private String processDefId;

    private Date startTime;

    private Date endTime;

    private String status;

    private String currentNodeIds;
    
    private String parentInstanceNo;

    private String parentNodeInstanceNo;

    private String bizStatus;

    private String bizData;

    private String vars;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    private String keyField;

    private String keyField2;
    public String getInstanceNo() {
        return instanceNo;
    }
    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }
    public String getInstanceName() {
        return instanceName;
    }
    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
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
    public String getCurrentNodeIds() {
        return currentNodeIds;
    }
    public void setCurrentNodeIds(String currentNodeIds) {
        this.currentNodeIds = currentNodeIds;
    }
    public String getParentInstanceNo() {
        return parentInstanceNo;
    }
    public void setParentInstanceNo(String parentInstanceNo) {
        this.parentInstanceNo = parentInstanceNo;
    }
    public String getParentNodeInstanceNo() {
        return parentNodeInstanceNo;
    }
    public void setParentNodeInstanceNo(String parentNodeInstanceNo) {
        this.parentNodeInstanceNo = parentNodeInstanceNo;
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
        return "ProcessInstanceDTO [instanceNo=" + instanceNo + ", instanceName=" + instanceName + ", processType="
                + processType + ", bizNo=" + bizNo + ", productCode=" + productCode + ", creator=" + creator
                + ", processDefId=" + processDefId + ", startTime=" + startTime + ", endTime=" + endTime + ", status="
                + status + ", currentNodeIds=" + currentNodeIds + ", parentInstanceNo=" + parentInstanceNo
                + ", parentNodeInstanceNo=" + parentNodeInstanceNo + ", bizStatus=" + bizStatus + ", bizData=" + bizData
                + ", vars=" + vars + ", extData=" + extData + ", createdDate=" + createdDate + ", modifiedDate="
                + modifiedDate + ", keyField=" + keyField + ", keyField2=" + keyField2 + "]";
    }
    
    
}
