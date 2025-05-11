package com.jd.easyflow.flow.ext.cases.timeout;

import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 */
public class TimeoutFilterTest {
    
    private static final Logger logger = LoggerFactory.getLogger(TimeoutFilterTest.class);

    @Test
    public void testNodeActionFilter() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/timeout/flow_timeout_001.json");
        flowEngine.init();
        
        FlowParam flowParam = new FlowParam("timeout_test_001");
        FlowResult flowResult = flowEngine.execute(flowParam);
        Thread.sleep(2000);
        
    }
    
    @Test
    public void testFlowFilter() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/timeout/flow_timeout_002.json");
        flowEngine.init();
        
        FlowParam flowParam = new FlowParam("timeout_test_002");
        FlowResult flowResult = flowEngine.execute(flowParam);
        Thread.sleep(2000);
        
    }
    
    public FlowResult onTimeout(Map<String, Object> config, FlowContext context) {
        logger.info("flow exeucte timeout");
        return context.getResult();
    }
}
