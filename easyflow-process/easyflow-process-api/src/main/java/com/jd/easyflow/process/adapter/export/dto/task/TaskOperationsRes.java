package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class TaskOperationsRes implements Serializable {
    
    public TaskOperationsRes() {
        
    }
    
    public TaskOperationsRes(List<TaskOperateCommandResult> resultList) {
        this.resultList = resultList;
    }

    private List<TaskOperateCommandResult> resultList;

    public List<TaskOperateCommandResult> getResultList() {
        return resultList;
    }

    public void setResultList(List<TaskOperateCommandResult> resultList) {
        this.resultList = resultList;
    }

    @Override
    public String toString() {
        return "TaskOperationsRes [resultList=" + resultList + "]";
    }
    
    
}
