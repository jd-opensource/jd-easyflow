package com.jd.easyflow.process.adapter.export.dto.task.command;

import java.util.List;

import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.task.TaskOperateCommand;

/**
 * @author liyuliang5
 *
 */
public class MultipleTaskCreateCommand extends TaskOperateCommand {

    public MultipleTaskCreateCommand() {
        this.commandType = ProcessTaskConstants.COMMAND_TYPE_CREATE_MULTIPLE_TASK;
    }
    
    private List<TaskCreateCommand> createCommandList;
    
    public MultipleTaskCreateCommand(List<TaskCreateCommand> createCommandList) {
        this();
        this.createCommandList = createCommandList;
    }

    public List<TaskCreateCommand> getCreateCommandList() {
        return createCommandList;
    }

    public void setCreateCommandList(List<TaskCreateCommand> createCommandList) {
        this.createCommandList = createCommandList;
    }

    @Override
    public String toString() {
        return "MultipleTaskCreateCommand [createCommandList=" + createCommandList + "]";
    }
    
    
}
