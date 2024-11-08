package com.jd.easyflow.process.infrastructure.persistence.po;

import java.util.Date;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTask {
    
    private Long id;

    private String taskNo;

    private String processType;

    private String bizNo;

    private String taskBizCode;
    
    private String taskBizName;

    private String taskType;

    private String processInstanceNo;

    private String nodeInstanceNo;

    private String nodeExecutionNo;

    private String assignType;

    private String assignInfo;

    private Date assignTime;

    private String executor;

    private String executeBizResult;

    private String executeBizData;

    private Date executeTime;

    private String productCode;
    
    private String creator;

    private String status;
    
    private String extData;

    private Date createdDate;

    private Date modifiedDate;

    private boolean deleted;
    
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

    public String getTaskBizName() {
        return taskBizName;
    }

    public void setTaskBizName(String taskBizName) {
        this.taskBizName = taskBizName;
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

    public String getNodeExecutionNo() {
        return nodeExecutionNo;
    }

    public void setNodeExecutionNo(String nodeExecutionNo) {
        this.nodeExecutionNo = nodeExecutionNo;
    }

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
    }

    public String getAssignInfo() {
        return assignInfo;
    }

    public void setAssignInfo(String assignInfo) {
        this.assignInfo = assignInfo;
    }

    public Date getAssignTime() {
        return assignTime;
    }

    public void setAssignTime(Date assignTime) {
        this.assignTime = assignTime;
    }

    public String getExecutor() {
        return executor;
    }

    public void setExecutor(String executor) {
        this.executor = executor;
    }

    public String getExecuteBizResult() {
        return executeBizResult;
    }

    public void setExecuteBizResult(String executeBizResult) {
        this.executeBizResult = executeBizResult;
    }

    public String getExecuteBizData() {
        return executeBizData;
    }

    public void setExecuteBizData(String executeBizData) {
        this.executeBizData = executeBizData;
    }

    public Date getExecuteTime() {
        return executeTime;
    }

    public void setExecuteTime(Date executeTime) {
        this.executeTime = executeTime;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
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
    
    
    
    public String getCreator() {
        return creator;
    }

    public void setCreator(String creator) {
        this.creator = creator;
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
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName());
        sb.append(" [");
        sb.append("Hash = ").append(hashCode());
        sb.append(", id=").append(id);
        sb.append(", taskNo=").append(taskNo);
        sb.append(", processType=").append(processType);
        sb.append(", bizNo=").append(bizNo);
        sb.append(", taskBizCode=").append(taskBizCode);
        sb.append(", taskBizName=").append(taskBizName);
        sb.append(", taskType=").append(taskType);
        sb.append(", processInstanceNo=").append(processInstanceNo);
        sb.append(", nodeInstanceNo=").append(nodeInstanceNo);
        sb.append(", nodeExecutionNo=").append(nodeExecutionNo);
        sb.append(", assignType=").append(assignType);
        sb.append(", assignInfo=").append(assignInfo);
        sb.append(", assignTime=").append(assignTime);
        sb.append(", executor=").append(executor);
        sb.append(", executeBizResult=").append(executeBizResult);
        sb.append(", executeBizData=").append(executeBizData);
        sb.append(", executeTime=").append(executeTime);
        sb.append(", productCode=").append(productCode);
        sb.append(", creator=").append(creator);
        sb.append(", status=").append(status);
        sb.append(", extData=").append(extData);
        sb.append(", createdDate=").append(createdDate);
        sb.append(", modifiedDate=").append(modifiedDate);
        sb.append(", deleted=").append(deleted);
        sb.append(", processInstanceKeyField=").append(processInstanceKeyField);
        sb.append(", processInstanceKeyField2=").append(processInstanceKeyField2);
        sb.append("]");
        return sb.toString();
    }
}