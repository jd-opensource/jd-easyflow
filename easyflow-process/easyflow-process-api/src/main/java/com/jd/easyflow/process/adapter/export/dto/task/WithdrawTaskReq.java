package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class WithdrawTaskReq implements Serializable {

    private String taskNo;

    private String user;
    
    private String withdrawInstancePolicy;

    private String instanceBizStatus;

    private String instanceBizData;

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

    public String getWithdrawInstancePolicy() {
        return withdrawInstancePolicy;
    }

    public void setWithdrawInstancePolicy(String withdrawInstancePolicy) {
        this.withdrawInstancePolicy = withdrawInstancePolicy;
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

    @Override
    public String toString() {
        return "WithdrawTaskReq [taskNo=" + taskNo + ", user=" + user + ", withdrawInstancePolicy="
                + withdrawInstancePolicy + ", instanceBizStatus=" + instanceBizStatus + ", instanceBizData="
                + instanceBizData + "]";
    }
    
    
    
}
