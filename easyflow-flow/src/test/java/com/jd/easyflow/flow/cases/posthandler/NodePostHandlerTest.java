package com.jd.easyflow.flow.cases.posthandler;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * Test NodePostHandler function
 * @author liyuliang5
 *
 */
public class NodePostHandlerTest {

    private static final Logger logger = LoggerFactory.getLogger(NodePostHandlerTest.class);
    
    /**
     * Test exp post
     */
    @Test
    public void testExpPostTo() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_exp_post_to_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_exp_post_to_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("STEP2", result.getContext().getEndNodes().get(0).getNodeId());        
    }
    
    @Test
    public void testPostParam() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_post_param_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_post_param_001");
        flowEngine.execute(param);
    }
}
