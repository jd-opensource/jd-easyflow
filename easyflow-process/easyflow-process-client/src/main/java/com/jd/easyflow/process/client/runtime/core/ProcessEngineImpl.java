package com.jd.easyflow.process.client.runtime.core;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * @author liyuliang5
 *
 */
public class ProcessEngineImpl implements ProcessEngine {

    private FlowEngine flowEngine;

    private FsmManager fsmManager;

    @Override
    public ProcessResult execute(ProcessParam param) {
        FlowParam flowParam = new FlowParam(param.getProcessId(), param.getNodeIds(), param.getParam(), param.getDataMap());
        FlowResult result = flowEngine.execute(flowParam);
        ProcessResult processResult = new ProcessResult();
        processResult.setResult(result.getResult());
        if (result.getContext() != null) {
            StdProcessContext processContext = result.getContext().get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
            if (processContext != null) {
                processResult.setProcessInstanceNo(processContext.getInstanceNo());
            }
        }
        
        return processResult;
    }

    public FlowEngine getFlowEngine() {
        return flowEngine;
    }

    public void setFlowEngine(FlowEngine flowEngine) {
        this.flowEngine = flowEngine;
    }

    public FsmManager getFsmManager() {
        return fsmManager;
    }

    public void setFsmManager(FsmManager fsmManager) {
        this.fsmManager = fsmManager;
    }

}
