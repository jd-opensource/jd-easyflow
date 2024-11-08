package com.jd.easyflow.process.client.task.service.dto;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TaskExecuteParam {

    private String user;

    private String taskNo;

    private String executeBizResult;

    private String executeBizData;

    private String instanceBizStatus;
    
    private String instanceBizData;

    private List<String> groupList;
    private List<String> group2List;
    private String cmdListStr;
    private StdProcessContext processContext;
    private StdNodeContext nodeContext;
    
    private Map<String, Object> flowTaskProperties;
    
    private Map<String, Object> taskProperties;
    
    private String taskBizCode;
    
    private String bizNo;
    
    private String processType;
    
    private String operation;
    
    private String taskExtData;

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

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

    public String getExecuteBizData() {
        return executeBizData;
    }

    public void setExecuteBizData(String executeBizData) {
        this.executeBizData = executeBizData;
    }

    public String getInstanceBizStatus() {
        return instanceBizStatus;
    }

    public void setInstanceBizStatus(String instanceBizStatus) {
        this.instanceBizStatus = instanceBizStatus;
    }

    public String getInstanceBizData() {
        return instanceBizData;
    }

    public void setInstanceBizData(String instanceBizData) {
        this.instanceBizData = instanceBizData;
    }

    public List<String> getGroupList() {
        return groupList;
    }

    public void setGroupList(List<String> groupList) {
        this.groupList = groupList;
    }

    public List<String> getGroup2List() {
        return group2List;
    }

    public void setGroup2List(List<String> group2List) {
        this.group2List = group2List;
    }

    public String getCmdListStr() {
        return cmdListStr;
    }

    public void setCmdListStr(String cmdListStr) {
        this.cmdListStr = cmdListStr;
    }

    public StdProcessContext getProcessContext() {
        return processContext;
    }

    public void setProcessContext(StdProcessContext processContext) {
        this.processContext = processContext;
    }

    public StdNodeContext getNodeContext() {
        return nodeContext;
    }

    public void setNodeContext(StdNodeContext nodeContext) {
        this.nodeContext = nodeContext;
    }

    public Map<String, Object> getFlowTaskProperties() {
        return flowTaskProperties;
    }

    public void setFlowTaskProperties(Map<String, Object> flowTaskProperties) {
        this.flowTaskProperties = flowTaskProperties;
    }

    public Map<String, Object> getTaskProperties() {
        return taskProperties;
    }

    public void setTaskProperties(Map<String, Object> taskProperties) {
        this.taskProperties = taskProperties;
    }

    public String getTaskBizCode() {
        return taskBizCode;
    }

    public void setTaskBizCode(String taskBizCode) {
        this.taskBizCode = taskBizCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public String getTaskExtData() {
        return taskExtData;
    }

    public void setTaskExtData(String taskExtData) {
        this.taskExtData = taskExtData;
    }

    @Override
    public String toString() {
        return "TaskExecuteParam [user=" + user + ", taskNo=" + taskNo + ", executeBizResult=" + executeBizResult
                + ", executeBizData=" + executeBizData + ", instanceBizStatus=" + instanceBizStatus
                + ", instanceBizData=" + instanceBizData + ", groupList=" + groupList + ", group2List=" + group2List
                + ", cmdListStr=" + cmdListStr + ", processContext=" + processContext + ", nodeContext=" + nodeContext
                + ", flowTaskProperties=" + flowTaskProperties + ", taskProperties=" + taskProperties + ", taskBizCode="
                + taskBizCode + ", bizNo=" + bizNo + ", processType=" + processType + ", operation=" + operation
                + ", taskExtData=" + taskExtData + "]";
    }
    
    


}
