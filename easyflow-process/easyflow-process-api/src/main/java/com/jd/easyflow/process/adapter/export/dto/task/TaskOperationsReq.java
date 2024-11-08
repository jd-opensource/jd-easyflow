package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class TaskOperationsReq implements Serializable {
    
    public TaskOperationsReq() {
        
    }
    
    public TaskOperationsReq(List<TaskOperateCommand> commandList) {
        this.commandList = commandList;
    }

    private List<TaskOperateCommand> commandList;

    public List<TaskOperateCommand> getCommandList() {
        return commandList;
    }

    public void setCommandList(List<TaskOperateCommand> commandList) {
        this.commandList = commandList;
    }

    @Override
    public String toString() {
        return "TaskOperationsReq [commandList=" + commandList + "]";
    }
    
}
