package com.jd.easyflow.flow.bpmn.cases.parallel;

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
public class BpmnParallelTest {

    /**
     * test bpmn parallel nodes.
     */
    @Test
    public void testParallel001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/parallel/flow_parallel001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_bpmn_parallel001");
        FlowResult result = flowEngine.execute(param);
    }
    
}
