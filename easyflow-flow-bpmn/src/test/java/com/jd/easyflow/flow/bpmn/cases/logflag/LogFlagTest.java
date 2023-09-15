package com.jd.easyflow.flow.bpmn.cases.logflag;

import org.junit.Test;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 */
public class LogFlagTest {

    /**
     * test log flag.
     */
    @Test
    public void testLogFlag001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/logflag/logflag_001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("Process_1");
        FlowResult result = flowEngine.execute(param);
    }
    
    /**
     * test log flag.
     */
    @Test
    public void testLogFlag002() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/logflag/logflag_001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("Process_1");
        param.setLogFlag(true);
        FlowResult result = flowEngine.execute(param);
    }
    
    /**
     * test log flag.
     */
    @Test
    public void testLogFlag003() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/logflag/logflag_001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("Process_1");
        param.setLogFlag(false);
        FlowResult result = flowEngine.execute(param);
    }
}
