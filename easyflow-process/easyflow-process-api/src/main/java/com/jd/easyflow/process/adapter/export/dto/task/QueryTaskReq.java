package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class QueryTaskReq implements Serializable {

    private String processType;
    
    private String bizNo;
    
    private String processInstanceNo;
    
    private String taskBizCode;
    
    private String nodeInstanceNo;
    
    private String status;

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

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getTaskBizCode() {
        return taskBizCode;
    }

    public void setTaskBizCode(String taskBizCode) {
        this.taskBizCode = taskBizCode;
    }

    public String getNodeInstanceNo() {
        return nodeInstanceNo;
    }

    public void setNodeInstanceNo(String nodeInstanceNo) {
        this.nodeInstanceNo = nodeInstanceNo;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "QueryTaskReq [processType=" + processType + ", bizNo=" + bizNo + ", processInstanceNo="
                + processInstanceNo + ", taskBizCode=" + taskBizCode + ", nodeInstanceNo=" + nodeInstanceNo
                + ", status=" + status + "]";
    }
    
    
}
