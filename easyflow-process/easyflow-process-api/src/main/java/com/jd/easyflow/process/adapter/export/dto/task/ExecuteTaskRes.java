package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class ExecuteTaskRes implements Serializable {

    private List<TaskOperateCmdResult> cmdResultList;

    public List<TaskOperateCmdResult> getCmdResultList() {
        return cmdResultList;
    }

    public void setCmdResultList(List<TaskOperateCmdResult> cmdResultList) {
        this.cmdResultList = cmdResultList;
    }

    @Override
    public String toString() {
        return "ExecuteTaskRes [cmdResultList=" + cmdResultList + "]";
    }
    
    
}
