package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class ProcessNodeExecutionDTO implements Serializable {

    private Long id;

    private String nodeExecutionNo;

    private String nodeInstanceNo;

    private String eventId;

    private String processDefId;

    private String nodeId;

    private Date startTime;

    private Date endTime;

    private String status;

    private String productCode;

    private String executor;

    private String nextNodeInstances;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getNodeExecutionNo() {
        return nodeExecutionNo;
    }

    public void setNodeExecutionNo(String nodeExecutionNo) {
        this.nodeExecutionNo = nodeExecutionNo;
    }

    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }

    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }

    public String getEventId() {
        return eventId;
    }

    public void setEventId(String eventId) {
        this.eventId = eventId;
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

    public String getExecutor() {
        return executor;
    }

    public void setExecutor(String executor) {
        this.executor = executor;
    }

    public String getNextNodeInstances() {
        return nextNodeInstances;
    }

    public void setNextNodeInstances(String nextNodeInstances) {
        this.nextNodeInstances = nextNodeInstances;
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
        return "ProcessNodeExecutionDTO [id=" + id + ", nodeExecutionNo=" + nodeExecutionNo + ", nodeInstanceNo="
                + nodeInstanceNo + ", eventId=" + eventId + ", processDefId=" + processDefId + ", nodeId=" + nodeId
                + ", startTime=" + startTime + ", endTime=" + endTime + ", status=" + status + ", productCode="
                + productCode + ", executor=" + executor + ", nextNodeInstances=" + nextNodeInstances + ", extData="
                + extData + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
}
