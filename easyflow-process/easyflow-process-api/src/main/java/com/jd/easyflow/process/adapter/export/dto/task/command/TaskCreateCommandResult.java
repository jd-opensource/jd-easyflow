package com.jd.easyflow.process.adapter.export.dto.task.command;

import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommandResult;

/**
 * @author liyuliang5
 *
 */
public class TaskCreateCommandResult extends TaskOperateCommandResult {
    
    public TaskCreateCommandResult() {
        this.commandType = ProcessTaskConstants.COMMAND_TYPE_CREATE_TASK;
    }

    private String taskNo;

    public String getTaskNo() {
        return taskNo;
    }

    public void setTaskNo(String taskNo) {
        this.taskNo = taskNo;
    }

    @Override
    public String toString() {
        return "TaskCreateCommandResult [taskNo=" + taskNo + "]";
    }
    
    
}
