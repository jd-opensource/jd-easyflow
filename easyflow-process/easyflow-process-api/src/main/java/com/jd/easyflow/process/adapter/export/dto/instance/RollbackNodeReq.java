package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 */
public class RollbackNodeReq implements Serializable {

    private String processInstanceNo;
    
    private String targetNodeId;
    
    private String targetNodeInstanceNo;
    
    private boolean rollbackSubProcess = true;
    
    private boolean rollbackTask = true;

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public String getTargetNodeInstanceNo() {
        return targetNodeInstanceNo;
    }

    public void setTargetNodeInstanceNo(String targetNodeInstanceNo) {
        this.targetNodeInstanceNo = targetNodeInstanceNo;
    }

    public String getTargetNodeId() {
        return targetNodeId;
    }

    public void setTargetNodeId(String targetNodeId) {
        this.targetNodeId = targetNodeId;
    }

    public boolean isRollbackSubProcess() {
        return rollbackSubProcess;
    }

    public void setRollbackSubProcess(boolean rollbackSubProcess) {
        this.rollbackSubProcess = rollbackSubProcess;
    }

    public boolean isRollbackTask() {
        return rollbackTask;
    }

    public void setRollbackTask(boolean rollbackTask) {
        this.rollbackTask = rollbackTask;
    }
    
    
    
}
