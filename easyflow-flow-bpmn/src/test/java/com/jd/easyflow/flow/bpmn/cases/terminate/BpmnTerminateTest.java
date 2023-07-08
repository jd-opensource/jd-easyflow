package com.jd.easyflow.flow.bpmn.cases.terminate;

import org.junit.Test;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * BPMN终止测试
 * @author liyuliang5
 *
 */
public class BpmnTerminateTest {

    /**
     * test bpmn terminate end event.
     */
    @Test
    public void testTerminate001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/terminate/flow_terminate001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_bpmn_terminate001");
        FlowResult result = flowEngine.execute(param);
    }
}
