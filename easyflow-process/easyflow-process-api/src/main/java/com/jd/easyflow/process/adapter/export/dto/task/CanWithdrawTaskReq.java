package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class CanWithdrawTaskReq implements Serializable {

    private String taskNo;

    private String user;
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
    @Override
    public String toString() {
        return "CanWithdrawTaskReq [taskNo=" + taskNo + ", user=" + user + "]";
    }
    
    
}
