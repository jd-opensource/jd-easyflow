package com.jd.easyflow.admin.process.adapter.page.dto;

/**
 * @author liyuliang5
 *
 */
public class CommonTaskExecuteDTO {

    private String taskNo;
    
    private String executeBizResult;
    
    private String instanceBizData;
    
    private String executeBizData;
    
    private String assignUserList;
    
    private String operation;

    private Integer version;
    public String getTaskNo() {
        return taskNo;
    }
    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }
    public String getExecuteBizResult() {
        return executeBizResult;
    }
    public void setExecuteBizResult(String executeBizResult) {
        this.executeBizResult = executeBizResult;
    }
    public String getInstanceBizData() {
        return instanceBizData;
    }
    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }
    public String getExecuteBizData() {
        return executeBizData;
    }
    public void setExecuteBizData(String executeBizData) {
        this.executeBizData = executeBizData;
    }
    public String getAssignUserList() {
        return assignUserList;
    }
    public void setAssignUserList(String assignUserList) {
        this.assignUserList = assignUserList;
    }
    public String getOperation() {
        return operation;
    }
    public void setOperation(String operation) {
        this.operation = operation;
    }
    public Integer getVersion() {
        return version;
    }
    public void setVersion(Integer version) {
        this.version = version;
    }
    @Override
    public String toString() {
        return "CommonTaskExecuteDTO [taskNo=" + taskNo + ", executeBizResult=" + executeBizResult
                + ", instanceBizData=" + instanceBizData + ", executeBizData=" + executeBizData + ", assignUserList="
                + assignUserList + ", operation=" + operation + ", version=" + version + "]";
    }
    
    
}
