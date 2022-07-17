package com.jd.easyflow.flow.cases.posthandler;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * Test PostHandlerFilter
 * 
 * @author liyuliang5
 *
 */
public class PostHandlerFilterTest {

    private static final Logger logger = LoggerFactory.getLogger(PostHandlerFilterTest.class);

    /**
     * Test exp post
     */
    @Test
    public void testPostHandlerFilter() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_post_handler_filter_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_post_handler_filter_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("STEP3", result.getContext().getEndNodes().get(0).getNodeId());
    }
}
