package com.jd.easyflow.process.domain.model.vo;

/**
 * @author liyuliang5
 *
 */
public class QueryTaskReqVO {

    private Long id;
    
    private String taskNo;
    
    private String processType;
    
    private String bizNo;
    
    private String taskBizCode;
    
    private String taskType;
    
    private String processInstanceNo;
    
    private String nodeInstanceNo;
    
    private String assignType;
    
    private String assignTimeStart;
    private String assignTimeEnd;
    
    private String executor;
    
    private String executeTimeStart;
    
    private String executeTimeEnd;
    
    private String productCode;
    
    private String creator;
    
    private String status;
    
    private String createdDateStart;
    
    private String createdDateEnd;
    
    private String modifiedDateStart;
    
    private String modifiedDateEnd;
    
    private String processInstanceKeyField;

    private String processInstanceKeyField2;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
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

    public String getTaskBizCode() {
        return taskBizCode;
    }

    public void setTaskBizCode(String taskBizCode) {
        this.taskBizCode = taskBizCode;
    }

    public String getTaskType() {
        return taskType;
    }

    public void setTaskType(String taskType) {
        this.taskType = taskType;
    }

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }

    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
    }

    public String getAssignTimeStart() {
        return assignTimeStart;
    }

    public void setAssignTimeStart(String assignTimeStart) {
        this.assignTimeStart = assignTimeStart;
    }

    public String getAssignTimeEnd() {
        return assignTimeEnd;
    }

    public void setAssignTimeEnd(String assignTimeEnd) {
        this.assignTimeEnd = assignTimeEnd;
    }

    public String getExecutor() {
        return executor;
    }

    public void setExecutor(String executor) {
        this.executor = executor;
    }

    public String getExecuteTimeStart() {
        return executeTimeStart;
    }

    public void setExecuteTimeStart(String executeTimeStart) {
        this.executeTimeStart = executeTimeStart;
    }

    public String getExecuteTimeEnd() {
        return executeTimeEnd;
    }

    public void setExecuteTimeEnd(String executeTimeEnd) {
        this.executeTimeEnd = executeTimeEnd;
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

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
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

    public String getProcessInstanceKeyField() {
        return processInstanceKeyField;
    }

    public void setProcessInstanceKeyField(String processInstanceKeyField) {
        this.processInstanceKeyField = processInstanceKeyField;
    }

    public String getProcessInstanceKeyField2() {
        return processInstanceKeyField2;
    }

    public void setProcessInstanceKeyField2(String processInstanceKeyField2) {
        this.processInstanceKeyField2 = processInstanceKeyField2;
    }

    @Override
    public String toString() {
        return "QueryTaskReqVO [id=" + id + ", taskNo=" + taskNo + ", processType=" + processType + ", bizNo=" + bizNo
                + ", taskBizCode=" + taskBizCode + ", taskType=" + taskType + ", processInstanceNo=" + processInstanceNo
                + ", nodeInstanceNo=" + nodeInstanceNo + ", assignType=" + assignType + ", assignTimeStart="
                + assignTimeStart + ", assignTimeEnd=" + assignTimeEnd + ", executor=" + executor
                + ", executeTimeStart=" + executeTimeStart + ", executeTimeEnd=" + executeTimeEnd + ", productCode="
                + productCode + ", creator=" + creator + ", status=" + status + ", createdDateStart=" + createdDateStart
                + ", createdDateEnd=" + createdDateEnd + ", modifiedDateStart=" + modifiedDateStart
                + ", modifiedDateEnd=" + modifiedDateEnd + ", processInstanceKeyField=" + processInstanceKeyField
                + ", processInstanceKeyField2=" + processInstanceKeyField2 + "]";
    }
    
    
}
