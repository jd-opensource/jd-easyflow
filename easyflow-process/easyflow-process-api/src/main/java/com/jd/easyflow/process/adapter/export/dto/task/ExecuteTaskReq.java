package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class ExecuteTaskReq implements Serializable {
    
    private String taskNo;

    private String user;

    private List<String> groupList;

    private List<String> group2List;

    private String executeBizResult;

    private String executeBizData;

    private String instanceBizStatus;

    private String instanceBizData;

    List<TaskOperateCmd> cmdList;

    private String operation;

    private String taskExtData;
    public String getTaskNo() {
        return taskNo;
    }
    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }
    public String getUser() {
        return user;
    }
    public void setUser(String user) {
        this.user = user;
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
    public List<TaskOperateCmd> getCmdList() {
        return cmdList;
    }
    public void setCmdList(List<TaskOperateCmd> cmdList) {
        this.cmdList = cmdList;
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
        return "ExecuteTaskReq [taskNo=" + taskNo + ", user=" + user + ", groupList=" + groupList + ", group2List="
                + group2List + ", executeBizResult=" + executeBizResult + ", executeBizData=" + executeBizData
                + ", instanceBizStatus=" + instanceBizStatus + ", instanceBizData=" + instanceBizData + ", cmdList="
                + cmdList + ", operation=" + operation + ", taskExtData=" + taskExtData + "]";
    }
    
    
    
}
