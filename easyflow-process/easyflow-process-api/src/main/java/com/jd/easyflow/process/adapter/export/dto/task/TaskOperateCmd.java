package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class TaskOperateCmd implements Serializable {

    protected String cmdType;

    public String getCmdType() {
        return cmdType;
    }

    public void setCmdType(String cmdType) {
        this.cmdType = cmdType;
    }

    @Override
    public String toString() {
        return "TaskOperateCmd [cmdType=" + cmdType + "]";
    }
    
    
       
}
