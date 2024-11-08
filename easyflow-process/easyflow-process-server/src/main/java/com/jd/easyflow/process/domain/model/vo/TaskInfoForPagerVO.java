package com.jd.easyflow.process.domain.model.vo;

/**
 * @author liyuliang5
 *
 */
public class TaskInfoForPagerVO {

    private String taskNo;
    
    private String processType;
    
    private String taskBizCode;
    
    private String taskBizName;
    
    private String bizNo;
    
    private String productCode;
    
    private String processInstanceNo;
    
    private String nodeInstanceNo;
    
    private String creator;
    
    private String status;
    
    private String executor;
    
    private String executeBizResult;
    
    private String executeBizData;
    
    private String executeTime;
    
    private String createdDate;
    
    private String modifiedDate;

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

    public String getExecuteTime() {
        return executeTime;
    }

    public void setExecuteTime(String executeTime) {
        this.executeTime = executeTime;
    }

    public String getCreatedDate() {
        return createdDate;
    }

    public void setCreatedDate(String createdDate) {
        this.createdDate = createdDate;
    }

    public String getModifiedDate() {
        return modifiedDate;
    }

    public void setModifiedDate(String modifiedDate) {
        this.modifiedDate = modifiedDate;
    }

    @Override
    public String toString() {
        return "TaskInfoForPagerVO [taskNo=" + taskNo + ", processType=" + processType + ", taskBizCode=" + taskBizCode
                + ", taskBizName=" + taskBizName + ", bizNo=" + bizNo + ", productCode=" + productCode
                + ", processInstanceNo=" + processInstanceNo + ", nodeInstanceNo=" + nodeInstanceNo + ", creator="
                + creator + ", status=" + status + ", executor=" + executor + ", executeBizResult=" + executeBizResult
                + ", executeBizData=" + executeBizData + ", executeTime=" + executeTime + ", createdDate=" + createdDate
                + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
    
}
