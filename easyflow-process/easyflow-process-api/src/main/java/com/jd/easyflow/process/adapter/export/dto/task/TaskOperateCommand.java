package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class TaskOperateCommand implements Serializable {

    protected String commandType;

    public String getCommandType() {
        return commandType;
    }

    public void setCommandType(String commandType) {
        this.commandType = commandType;
    }

    @Override
    public String toString() {
        return "TaskOperateCommand [commandType=" + commandType + "]";
    }
    
    
    
}
