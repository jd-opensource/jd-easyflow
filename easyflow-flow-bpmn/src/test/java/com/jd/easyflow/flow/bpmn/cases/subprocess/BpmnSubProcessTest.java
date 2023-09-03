package com.jd.easyflow.flow.bpmn.cases.subprocess;

import org.junit.Test;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * Bpmn sub flow test.
 * @author liyuliang5
 *
 */
public class BpmnSubProcessTest {

    /**
     * test bpmn sub flow.
     */
    @Test
    public void testSubFlow001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/subprocess/*.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("subprocess_test_001");
        FlowResult result = flowEngine.execute(param);
    }
}
