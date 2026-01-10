package com.jd.easyflow.processunit.client.bean;

import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;

/**
 * @author liyuliang5
 */
public class ProcessUnitCreateAndExecuteReq {

    private ProcessUnitCreateReq createReq;
    
    private ExecParam execParam;
    
    public ProcessUnitCreateAndExecuteReq() {
        
    }
    
    public ProcessUnitCreateAndExecuteReq(ProcessUnitCreateReq createReq, ExecParam execParam) {
        this.createReq = createReq;
        this.execParam = execParam;
    }

    public ProcessUnitCreateReq getCreateReq() {
        return createReq;
    }

    public void setCreateReq(ProcessUnitCreateReq createReq) {
        this.createReq = createReq;
    }

    public ExecParam getExecParam() {
        return execParam;
    }

    public void setExecParam(ExecParam execParam) {
        this.execParam = execParam;
    }
    
    
}
