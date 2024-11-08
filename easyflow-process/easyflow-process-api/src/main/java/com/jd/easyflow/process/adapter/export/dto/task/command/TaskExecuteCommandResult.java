package com.jd.easyflow.process.adapter.export.dto.task.command;

import java.util.List;

import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommandResult;

/**
 * @author liyuliang5
 *
 */
public class TaskExecuteCommandResult extends TaskOperateCommandResult {
    
    public TaskExecuteCommandResult() {
        this.commandType = ProcessTaskConstants.COMMAND_TYPE_EXECUTE_TASK;
    }

    private List<TaskOperateCommandResult> subResultList;

    public List<TaskOperateCommandResult> getSubResultList() {
        return subResultList;
    }

    public void setSubResultList(List<TaskOperateCommandResult> subResultList) {
        this.subResultList = subResultList;
    }

    @Override
    public String toString() {
        return "TaskExecuteCommandResult [subResultList=" + subResultList + "]";
    }
    
    
}
