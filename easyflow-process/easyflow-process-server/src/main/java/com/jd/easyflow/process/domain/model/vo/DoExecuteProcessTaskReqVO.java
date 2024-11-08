package com.jd.easyflow.process.domain.model.vo;

import java.util.List;

import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommand;

/**
 * @author liyuliang5
 *
 */
public class DoExecuteProcessTaskReqVO {
    
    private String taskNo;
    
    private String executor;
    
    private List<String> groupList;
    
    private List<String> group2List;
    
    private String executeBizResult;
    
    private String executeBizData;
    
    private String instanceBizStatus;
    
    private String instanceBizData;
    
    private List<TaskOperateCommand> subCommandList;
    
    private String operation;
    
    private String taskExtData;

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    public String getExecutor() {
        return executor;
    }

    public void setExecutor(String executor) {
        this.executor = executor;
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

    public List<TaskOperateCommand> getSubCommandList() {
        return subCommandList;
    }

    public void setSubCommandList(List<TaskOperateCommand> subCommandList) {
        this.subCommandList = subCommandList;
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
        return "DoExecuteProcessTaskReqVO [taskNo=" + taskNo + ", executor=" + executor + ", groupList=" + groupList
                + ", group2List=" + group2List + ", executeBizResult=" + executeBizResult + ", executeBizData="
                + executeBizData + ", instanceBizStatus=" + instanceBizStatus + ", instanceBizData=" + instanceBizData
                + ", subCommandList=" + subCommandList + ", operation=" + operation + ", taskExtData=" + taskExtData
                + "]";
    }

    
}
