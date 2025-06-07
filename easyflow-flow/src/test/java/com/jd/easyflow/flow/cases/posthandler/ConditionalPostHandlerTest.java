package com.jd.easyflow.flow.cases.posthandler;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.builder.FlowBuilder;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;

/**
 * @author liyuliang5
 */
public class ConditionalPostHandlerTest {
    
    private static final Logger logger = LoggerFactory.getLogger(ConditionalPostHandlerTest.class);
    
    @Test
    public void testPerformance() {
        ConditionalNodePostHandler handler = new ConditionalNodePostHandler();
        NodeContext nodeContext = new NodeContext("123");
        FlowContextImpl flowContext = new FlowContextImpl();
        Flow flow = FlowBuilder.create("001", "001").addNode("123", null).build();
        flowContext.setFlow(flow);
        flowContext.setFlowEngine(new FlowEngineImpl());
        Map<String, Object> branch = new HashMap<String, Object>();
        branch.put("false", "NEXT");
        handler.setBranchList(Arrays.asList(branch));
        //handler.init(null, null);
        handler.postHandle(nodeContext, flowContext);
        long t1 = System.currentTimeMillis();
        for (int i = 0; i < 1; i++) {
            handler.postHandle(nodeContext, flowContext);
        }
        logger.info("time cost" + (System.currentTimeMillis()  - t1));
        
    }
    
    @Test
    public void testCreateExpWhen() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/posthandler/flow_when_createexp_001.json");
        flowEngine.init();
        FlowResult result = flowEngine.execute(new FlowParam("flow_when_createexp_001"));
        assertEquals(result.getContext().getEndNodes().get(0).getNodeId(), "002");
    }

}
