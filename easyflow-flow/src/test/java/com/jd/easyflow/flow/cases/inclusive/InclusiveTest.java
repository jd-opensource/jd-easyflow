package com.jd.easyflow.flow.cases.inclusive;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowUtil;

public class InclusiveTest {

    @Test
    public void testInclusive001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive201.json");
        flowEngine.init();
        // false false
        FlowParam param = new FlowParam("flow_inclusive201");
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
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive201.json");
        flowEngine.init();
        // true false
        FlowParam param = new FlowParam("flow_inclusive201");
        param.setParam(1);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(1, endNodeIds.size());
        assertTrue(endNodeIds.contains("END"));
        NodeContext endNode = result.getContext().getEndNodes().stream().filter(node->node.getNodeId().equals("END")).findFirst().get();
        List<NodeContext> previousNodeList = endNode.getPreviousNode().get(FlowConstants.NODECTX_PREVIOUS_NODES);
        assertEquals(1, previousNodeList.size());
        assertEquals("NODE2", previousNodeList.get(0).getNodeId());
    }
    
    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive003() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive201.json");
        flowEngine.init();
        // true true
        FlowParam param = new FlowParam("flow_inclusive201");
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
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive202.json");
        flowEngine.init();
        // true false
        FlowParam param = new FlowParam("flow_inclusive202");
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
    public void testInclusive005() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive202.json");
        flowEngine.init();
        // true true
        FlowParam param = new FlowParam("flow_inclusive202");
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
    public void testInclusive006() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive203.json");
        flowEngine.init();
        // true true true
        FlowParam param = new FlowParam("flow_inclusive203");
        Map<String, Object> bizParam = new HashMap<>();
        param.setParam(bizParam);
        bizParam.put("node1", true);
        bizParam.put("node2", true);
        bizParam.put("node3", true);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertTrue(endNodeIds.contains("END"));
    }
    
    /**
     * test inclusive nodes.
     */
    @Test
    public void testInclusive007() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive203.json");
        flowEngine.init();
        // false true true
        FlowParam param = new FlowParam("flow_inclusive203");
        Map<String, Object> bizParam = new HashMap<>();
        param.setParam(bizParam);
        bizParam.put("node1", false);
        bizParam.put("node2", true);
        bizParam.put("node3", true);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertTrue(endNodeIds.contains("END"));
    }
    
    /**
     * test inclusive nodes.
     * dead loop
     */
    @Test
    public void testInclusive008() {
//        FlowEngineImpl flowEngine = new FlowEngineImpl();
//        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive303.json");
//        flowEngine.init();
//        // true false
//        FlowParam param = new FlowParam("flow_inclusive303");
//        Map<String, Object> bizParam = new HashMap<>();
//        param.setParam(bizParam);
//        bizParam.put("I1", false);
//        bizParam.put("I2", true);
//        FlowResult result = flowEngine.execute(param);
//        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
//        assertTrue(endNodeIds.contains("END"));
    }
    
    /**
     * test inclusive nodes.
     * dependency each other, not inactive inclusive gateway.
     */
    @Test
    public void testInclusive009() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/inclusive/flow_inclusive303.json");
        flowEngine.init();
        // true true
        FlowParam param = new FlowParam("flow_inclusive303");
        Map<String, Object> bizParam = new HashMap<>();
        param.setParam(bizParam);
        bizParam.put("I1", true);
        bizParam.put("I2", true);
        FlowResult result = flowEngine.execute(param);
        List<String> endNodeIds = FlowUtil.nodeIdsOfNodeContextList(result.getContext().getEndNodes());
        assertEquals(endNodeIds.size(), 2);
        assertTrue(endNodeIds.contains("I1"));
        assertTrue(endNodeIds.contains("I2"));
    }
    

}
