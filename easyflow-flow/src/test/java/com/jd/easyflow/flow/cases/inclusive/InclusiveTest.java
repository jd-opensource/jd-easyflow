package com.jd.easyflow.flow.cases.inclusive;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.util.FlowUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class InclusiveTest {

    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive001.json");
        flowEngine.init();
        // false false
        FlowParam param = new FlowParam("flow_inclusive001");
        param.setParam(3);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("START_NODE"));
        

        
       
    }
    
    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive002() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive001.json");
        flowEngine.init();
        // true false
        FlowParam param = new FlowParam("flow_inclusive001");
        param.setParam(1);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("END"));
    }
    
    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive003() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive001.json");
        flowEngine.init();
        // true true
        FlowParam param = new FlowParam("flow_inclusive001");
        param.setParam(0);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(2, endNodeIds.size());
        assertTrue(endNodeIds.contains("END"));
    }
    
    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive004() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive002.json");
        flowEngine.init();
        // true true
        FlowParam param = new FlowParam("flow_inclusive002");
        param.setParam(1);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("END"));
    }
}
