package com.jd.easyflow.flow.bpmn.cases.inclusive;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.util.FlowUtil;

/**
 * BPMN inclusive test.
 * @author liyuliang5
 *
 */
public class BpmnInclusiveTest {

    /**
     * test bpmn inclusive nodes.
     */
    @Test
    public void testInclusive001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive001.bpmn");
        flowEngine.init();
        
        FlowParam param = new FlowParam("flow_bpmn_inclusive001");
        param.setParam(1);
        
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("END"));
        
    }
    
}
