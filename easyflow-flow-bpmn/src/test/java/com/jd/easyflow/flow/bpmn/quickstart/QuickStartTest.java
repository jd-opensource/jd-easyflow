package com.jd.easyflow.flow.bpmn.quickstart;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class QuickStartTest {
    
    private static final Logger logger = LoggerFactory.getLogger(QuickStartTest.class);

    @Test
    public void testQuickStart001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/quickstart/quickstart_001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("quickstart_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }
}
