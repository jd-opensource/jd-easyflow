package com.jd.easyflow.flow.bpmn.cases.callactivity;

import org.junit.Test;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnCallActivityTest {
    /**
     * test bpmn call activity.
     */
    @Test
    public void testCallActivity001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/callactivity/*.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("callactivity_test_001");
        FlowResult result = flowEngine.execute(param);
    }
}

