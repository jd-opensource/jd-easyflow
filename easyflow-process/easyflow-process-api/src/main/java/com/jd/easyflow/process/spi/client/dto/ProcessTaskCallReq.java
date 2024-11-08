package com.jd.easyflow.process.spi.client.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskCallReq implements Serializable {

    private String processType;
    
    private String bizNo;
    
    private String taskBizCode;
    
    private String processInstanceNo;
    
    private String instanceBizData;
    
    private String instanceBizStatus;
    
    private String event;
    
    private String taskNo;

    private Map<String, Object> callParam;

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

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    public Map<String, Object> getCallParam() {
        return callParam;
    }

    public void setCallParam(Map<String, Object> callParam) {
        this.callParam = callParam;
    }

    @Override
    public String toString() {
        return "ProcessTaskCallReq [processType=" + processType + ", bizNo=" + bizNo + ", taskBizCode=" + taskBizCode
                + ", processInstanceNo=" + processInstanceNo + ", instanceBizData=" + instanceBizData
                + ", instanceBizStatus=" + instanceBizStatus + ", event=" + event + ", taskNo=" + taskNo
                + ", callParam=" + callParam + "]";
    }
    
    
}
