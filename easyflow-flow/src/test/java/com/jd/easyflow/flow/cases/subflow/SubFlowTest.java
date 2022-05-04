package com.jd.easyflow.flow.cases.subflow;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * Sub flow test cases.
 * 
 * @author liyuliang5
 *
 */
public class SubFlowTest {

    private static final Logger logger = LoggerFactory.getLogger(SubFlowTest.class);

    /**
     * Test inner subflow.
     */
    @Test
    public void testInnerSubFlow() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/subflow/flow_subflow_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("subflow_test_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }

    /**
     * Test out subflow.
     */
    @Test
    public void testOutSubFlow() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath(
                "classpath:flow/cases/subflow/flow_subflow_002.json,classpath:flow/cases/subflow/flow_subflow_002_01.json");
        flowEngine.init();
        FlowParam param = new FlowParam("subflow_test_002");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }

    /**
     * Test self subflow.
     */
    @Test
    public void testSelfSubFlow() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath(
                "classpath:flow/cases/subflow/flow_subflow_003.json,classpath:flow/cases/subflow/flow_subflow_003_01.json");
        flowEngine.init();
        FlowParam param = new FlowParam("subflow_test_003");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }
}
