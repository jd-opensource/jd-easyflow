package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskDTO implements Serializable {
    
    private String taskNo;

    private String processType;

    private String bizNo;

    private String taskBizCode;

    private String taskBizName;

    private String taskType;

    private String processInstanceNo;

    private String nodeInstanceNo;

    private String nodeExecutionNo;

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

    private List<ProcessTaskAssignDTO> assignList;
    
    private ProcessInstanceDTO processInstance;

    private String nodeTaskConf;
    
    private String assignInfo;
    
    private String assignType;

    private String processInstanceKeyField;

    private String processInstanceKeyField2;

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

    public List<ProcessTaskAssignDTO> getAssignList() {
        return assignList;
    }

    public void setAssignList(List<ProcessTaskAssignDTO> assignList) {
        this.assignList = assignList;
    }

    public ProcessInstanceDTO getProcessInstance() {
        return processInstance;
    }

    public void setProcessInstance(ProcessInstanceDTO processInstance) {
        this.processInstance = processInstance;
    }

    public String getNodeTaskConf() {
        return nodeTaskConf;
    }

    public void setNodeTaskConf(String nodeTaskConf) {
        this.nodeTaskConf = nodeTaskConf;
    }

    public String getAssignInfo() {
        return assignInfo;
    }

    public void setAssignInfo(String assignInfo) {
        this.assignInfo = assignInfo;
    }

    public String getAssignType() {
        return assignType;
    }

    public void setAssignType(String assignType) {
        this.assignType = assignType;
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
        return "ProcessTaskDTO [taskNo=" + taskNo + ", processType=" + processType + ", bizNo=" + bizNo
                + ", taskBizCode=" + taskBizCode + ", taskBizName=" + taskBizName + ", taskType=" + taskType
                + ", processInstanceNo=" + processInstanceNo + ", nodeInstanceNo=" + nodeInstanceNo
                + ", nodeExecutionNo=" + nodeExecutionNo + ", assignTime=" + assignTime + ", executor=" + executor
                + ", executeBizResult=" + executeBizResult + ", executeBizData=" + executeBizData + ", executeTime="
                + executeTime + ", productCode=" + productCode + ", creator=" + creator + ", status=" + status
                + ", extData=" + extData + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate
                + ", assignList=" + assignList + ", processInstance=" + processInstance + ", nodeTaskConf="
                + nodeTaskConf + ", assignInfo=" + assignInfo + ", assignType=" + assignType
                + ", processInstanceKeyField=" + processInstanceKeyField + ", processInstanceKeyField2="
                + processInstanceKeyField2 + "]";
    }
    
    
    
}
