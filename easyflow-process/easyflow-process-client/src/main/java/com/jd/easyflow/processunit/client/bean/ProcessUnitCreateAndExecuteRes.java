package com.jd.easyflow.processunit.client.bean;

import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;

/**
 * @author liyuliang5
 */
public class ProcessUnitCreateAndExecuteRes {
    
    private ProcessUnitCreateRes createRes;
    
    private ExecResult execResult;

    public ProcessUnitCreateRes getCreateRes() {
        return createRes;
    }

    public void setCreateRes(ProcessUnitCreateRes createRes) {
        this.createRes = createRes;
    }

    public ExecResult getExecResult() {
        return execResult;
    }

    public void setExecResult(ExecResult execResult) {
        this.execResult = execResult;
    }
    
}
