package com.jd.easyflow.process.adapter.message;

import java.io.Serializable;
import java.util.Date;

import com.jd.easyflow.common.adapter.message.BaseMessage;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTaskStatusMessage extends BaseMessage implements Serializable {

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

    private Date executeTime;

    private String executeBizResult;

    private String executeBizData;
    
    private String productCode;
    
    private String creator;

    private String status;
        
    private String extData;
    
    private Date createdDate;
    
    private Date modifiedDate;

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

    public Date getExecuteTime() {
        return executeTime;
    }

    public void setExecuteTime(Date executeTime) {
        this.executeTime = executeTime;
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
        return "ProcessTaskStatusMessage [taskNo=" + taskNo + ", processType=" + processType + ", bizNo=" + bizNo
                + ", taskBizCode=" + taskBizCode + ", taskBizName=" + taskBizName + ", taskType=" + taskType
                + ", processInstanceNo=" + processInstanceNo + ", nodeInstanceNo=" + nodeInstanceNo
                + ", nodeExecutionNo=" + nodeExecutionNo + ", assignType=" + assignType + ", assignInfo=" + assignInfo
                + ", assignTime=" + assignTime + ", executor=" + executor + ", executeTime=" + executeTime
                + ", executeBizResult=" + executeBizResult + ", executeBizData=" + executeBizData + ", productCode="
                + productCode + ", creator=" + creator + ", status=" + status + ", extData=" + extData
                + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate + "]";
    }
    
    
}
