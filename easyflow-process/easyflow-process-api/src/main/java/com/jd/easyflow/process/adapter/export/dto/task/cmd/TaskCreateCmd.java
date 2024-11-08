package com.jd.easyflow.process.adapter.export.dto.task.cmd;

import java.util.Map;

import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCmd;

/**
 * @author liyuliang5
 *
 */
public class TaskCreateCmd extends TaskOperateCmd {
    
    public TaskCreateCmd() {
        this.cmdType = ProcessTaskConstants.CMD_TYPE_CREATE_TASK;
    }
   
    private Map<String, Object> assignInfo;

    public Map<String, Object> getAssignInfo() {
        return assignInfo;
    }

    public void setAssignInfo(Map<String, Object> assignInfo) {
        this.assignInfo = assignInfo;
    }

    @Override
    public String toString() {
        return "TaskCreateCmd [assignInfo=" + assignInfo + "]";
    }
    
    
        
}
