package com.jd.easyflow.flow.cases.posthandler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeContext;

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
    
    @Test
    public void testCreateExpPostTo() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_createexp_post_to_001.json");
        flowEngine.init();
        FlowResult result = flowEngine.execute(new FlowParam("flow_createexp_post_to_001"));
        assertEquals(result.getContext().getEndNodes().get(0).getNodeId(), "STEP2");
    }
    
    @Test
    public void testPostData() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_postdata_001.json");
        flowEngine.init();
        FlowResult result = flowEngine.execute(new FlowParam("flow_postdata_001"));
        NodeContext nodeContext = result.getContext().getEndNodes().get(0);
        assertTrue((int)nodeContext.get("a") == 1);
        assertTrue((int)nodeContext.get("b") == 3);
        assertTrue((int) ((Map)nodeContext.get("c")).get("d") == 3);
        assertTrue(nodeContext.get("d").equals("dd"));
    }
}
