package com.jd.easyflow.admin.process.adapter.page.dto;

import java.util.Date;
import java.util.List;

import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskInfoForPagerDTO {


    private String taskNo;

    private String processType;

    private String bizNo;

    private String taskBizCode;
    
    private String taskBizName;

    private String taskType;

    private String processInstanceNo;
    
    private String processInstanceCreator;

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

    private String status;
    
    private String bizData;

    private String extData;

    private Date createdDate;

    private Date modifiedDate;
    
    private String instanceName;
    
    private String instanceStatus;
    
    private String instanceCreator;
    
    private Date instanceCreatedDate;
    
    private String instanceKeyField;

    private String instanceKeyField2;

    private List<ProcessTaskAssignDTO> assignList;
    
    private boolean canHandle;
    
    private boolean canWithdraw;

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

    public String getProcessInstanceCreator() {
        return processInstanceCreator;
    }

    public void setProcessInstanceCreator(String processInstanceCreator) {
        this.processInstanceCreator = processInstanceCreator;
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

    public String getBizData() {
        return bizData;
    }

    public void setBizData(String bizData) {
        this.bizData = bizData;
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

    public String getInstanceName() {
        return instanceName;
    }

    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }

    public String getInstanceStatus() {
        return instanceStatus;
    }

    public void setInstanceStatus(String instanceStatus) {
        this.instanceStatus = instanceStatus;
    }

    public String getInstanceCreator() {
        return instanceCreator;
    }

    public void setInstanceCreator(String instanceCreator) {
        this.instanceCreator = instanceCreator;
    }

    public Date getInstanceCreatedDate() {
        return instanceCreatedDate;
    }

    public void setInstanceCreatedDate(Date instanceCreatedDate) {
        this.instanceCreatedDate = instanceCreatedDate;
    }

    public String getInstanceKeyField() {
        return instanceKeyField;
    }

    public void setInstanceKeyField(String instanceKeyField) {
        this.instanceKeyField = instanceKeyField;
    }

    public String getInstanceKeyField2() {
        return instanceKeyField2;
    }

    public void setInstanceKeyField2(String instanceKeyField2) {
        this.instanceKeyField2 = instanceKeyField2;
    }

    public List<ProcessTaskAssignDTO> getAssignList() {
        return assignList;
    }

    public void setAssignList(List<ProcessTaskAssignDTO> assignList) {
        this.assignList = assignList;
    }

    public boolean isCanHandle() {
        return canHandle;
    }

    public void setCanHandle(boolean canHandle) {
        this.canHandle = canHandle;
    }

    public boolean isCanWithdraw() {
        return canWithdraw;
    }

    public void setCanWithdraw(boolean canWithdraw) {
        this.canWithdraw = canWithdraw;
    }

    @Override
    public String toString() {
        return "ProcessTaskInfoForPagerDTO [taskNo=" + taskNo + ", processType=" + processType + ", bizNo=" + bizNo
                + ", taskBizCode=" + taskBizCode + ", taskBizName=" + taskBizName + ", taskType=" + taskType
                + ", processInstanceNo=" + processInstanceNo + ", processInstanceCreator=" + processInstanceCreator
                + ", nodeInstanceNo=" + nodeInstanceNo + ", nodeExecutionNo=" + nodeExecutionNo + ", assignType="
                + assignType + ", assignInfo=" + assignInfo + ", assignTime=" + assignTime + ", executor=" + executor
                + ", executeBizResult=" + executeBizResult + ", executeBizData=" + executeBizData + ", executeTime="
                + executeTime + ", productCode=" + productCode + ", status=" + status + ", bizData=" + bizData
                + ", extData=" + extData + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate
                + ", instanceName=" + instanceName + ", instanceStatus=" + instanceStatus + ", instanceCreator="
                + instanceCreator + ", instanceCreatedDate=" + instanceCreatedDate + ", instanceKeyField="
                + instanceKeyField + ", instanceKeyField2=" + instanceKeyField2 + ", assignList=" + assignList
                + ", canHandle=" + canHandle + ", canWithdraw=" + canWithdraw + "]";
    }
    
    
}
