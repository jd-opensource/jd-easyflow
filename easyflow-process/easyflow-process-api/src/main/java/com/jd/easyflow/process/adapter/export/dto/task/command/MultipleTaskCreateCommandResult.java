package com.jd.easyflow.process.adapter.export.dto.task.command;

import java.util.List;

import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommandResult;

/**
 * @author liyuliang5
 *
 */
public class MultipleTaskCreateCommandResult extends TaskOperateCommandResult {

    public MultipleTaskCreateCommandResult() {
        this.commandType = ProcessTaskConstants.COMMAND_TYPE_CREATE_MULTIPLE_TASK;
    }
    
    private List<TaskCreateCommandResult> createResultList;
    
    public MultipleTaskCreateCommandResult(List<TaskCreateCommandResult> createResultList) {
        this();
        this.createResultList = createResultList;
    }

    public List<TaskCreateCommandResult> getCreateResultList() {
        return createResultList;
    }

    public void setCreateResultList(List<TaskCreateCommandResult> createResultList) {
        this.createResultList = createResultList;
    }

    @Override
    public String toString() {
        return "MultipleTaskCreateCommandResult [createResultList=" + createResultList + "]";
    }
    
    
}
