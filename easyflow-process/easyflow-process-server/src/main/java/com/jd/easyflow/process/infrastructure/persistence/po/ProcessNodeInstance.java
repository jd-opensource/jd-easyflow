package com.jd.easyflow.process.infrastructure.persistence.po;

import java.util.Date;

/**
 * @author liyuliang5
 * 
 */
public class ProcessNodeInstance {
    
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

    private boolean deleted;

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

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }
    
    

    public String getPreviousNodeInstances() {
        return previousNodeInstances;
    }

    public void setPreviousNodeInstances(String previousNodeInstances) {
        this.previousNodeInstances = previousNodeInstances;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", nodeInstanceNo=").append(nodeInstanceNo);
        sb.append(", processInstanceNo=").append(processInstanceNo);
        sb.append(", processDefId=").append(processDefId);
        sb.append(", nodeId=").append(nodeId);
        sb.append(", startTime=").append(startTime);
        sb.append(", endTime=").append(endTime);
        sb.append(", status=").append(status);
        sb.append(", productCode=").append(productCode);
        sb.append(", executors=").append(executors);
        sb.append(", previousNodeInstances=").append(previousNodeInstances);
        sb.append(", nextNodeInstances=").append(nextNodeInstances);
        sb.append(", vars=").append(vars);
        sb.append(", extData=").append(extData);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append("]");
        return sb.toString();
    }
}