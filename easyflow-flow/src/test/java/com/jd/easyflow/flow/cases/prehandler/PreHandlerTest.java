package com.jd.easyflow.flow.cases.prehandler;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class PreHandlerTest {
    
    private static final Logger logger = LoggerFactory.getLogger(PreHandlerTest.class);


    /**
     * Test exp post
     */
    @Test
    public void testPreHandler001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/prehandler/flow_prehandler_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_prehandler_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
    }
}
