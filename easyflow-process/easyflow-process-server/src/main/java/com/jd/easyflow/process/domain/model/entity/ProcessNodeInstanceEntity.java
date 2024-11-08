package com.jd.easyflow.process.domain.model.entity;

import java.util.Date;

/**
 * @author liyuliang5
 * 
 */
public class ProcessNodeInstanceEntity {
    
    private Long id;

    private String nodeInstanceNo;

    private String processInstanceNo;

    private String processDefId;

    private String nodeId;

    private Date startTime;

    private Date endTime;

    private String status;

    private String productCode;

    private String executors;
    
    private String previousNodeInstances;

    private String nextNodeInstances;

    private String vars;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }

    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getProcessDefId() {
        return processDefId;
    }

    public void setProcessDefId(String processDefId) {
        this.processDefId = processDefId;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
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

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getExecutors() {
        return executors;
    }

    public void setExecutors(String executors) {
        this.executors = executors;
    }

    public String getPreviousNodeInstances() {
        return previousNodeInstances;
    }

    public void setPreviousNodeInstances(String previousNodeInstances) {
        this.previousNodeInstances = previousNodeInstances;
    }

    public String getNextNodeInstances() {
        return nextNodeInstances;
    }

    public void setNextNodeInstances(String nextNodeInstances) {
        this.nextNodeInstances = nextNodeInstances;
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

    @Override
    public String toString() {
        return "ProcessNodeInstanceEntity [id=" + id + ", nodeInstanceNo=" + nodeInstanceNo + ", processInstanceNo="
                + processInstanceNo + ", processDefId=" + processDefId + ", nodeId=" + nodeId + ", startTime="
                + startTime + ", endTime=" + endTime + ", status=" + status + ", productCode=" + productCode
                + ", executors=" + executors + ", previousNodeInstances=" + previousNodeInstances
                + ", nextNodeInstances=" + nextNodeInstances + ", vars=" + vars + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
}