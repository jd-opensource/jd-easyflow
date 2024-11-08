package com.jd.easyflow.process.client.task.biz.dto;

import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class TaskBizParam {
    
    private String processType;
    
    private String bizNo;
    
    private String taskBizCode;
    
    private String processInstanceNo;
    
    private String instanceBizData;
    
    private String instanceBizStatus;
    
    private String event;

    private Map<String, Object> bizServiceParam;
    
    private String taskNo;
    
    private String operation;

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

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getInstanceBizData() {
        return instanceBizData;
    }

    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }

    public String getInstanceBizStatus() {
        return instanceBizStatus;
    }

    public void setInstanceBizStatus(String instanceBizStatus) {
        this.instanceBizStatus = instanceBizStatus;
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public Map<String, Object> getBizServiceParam() {
        return bizServiceParam;
    }

    public void setBizServiceParam(Map<String, Object> bizServiceParam) {
        this.bizServiceParam = bizServiceParam;
    }

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    @Override
    public String toString() {
        return "TaskBizParam [processType=" + processType + ", bizNo=" + bizNo + ", taskBizCode=" + taskBizCode
                + ", processInstanceNo=" + processInstanceNo + ", instanceBizData=" + instanceBizData
                + ", instanceBizStatus=" + instanceBizStatus + ", event=" + event + ", bizServiceParam="
                + bizServiceParam + ", taskNo=" + taskNo + ", operation=" + operation + "]";
    }
    
    
}
