package com.jd.easyflow.flow.bpmn.cases.defaultflow;

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
 * 
 * @author liyuliang5
 */
public class DefaultFlowTest {

    /**
     * test bpmn default flow.
     */
    @Test
    public void testDefaultFlow001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/defaultflow/default_flow_001.bpmn");
        flowEngine.init();
        
        FlowParam param = new FlowParam("default_flow_001");        
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("node1"));
        
    }
}
