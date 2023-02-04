package com.jd.easyflow.flow.cases.runner;

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
public class MultiThreadTest {
    
    public static final Logger logger = LoggerFactory.getLogger(MultiThreadTest.class);


    @Test
    public void test1() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/runner/flow_multi001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_multi001", "EMPTY_NODE", null);
        FlowResult result = flowEngine.execute(param);
        logger.info("end");
    }
    
    @Test(expected = Exception.class)
    public void testException() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/runner/flow_multi002.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_multi002", "EMPTY_NODE", null);
        FlowResult result = flowEngine.execute(param);
        logger.info("end");  
    }
    
    @Test
    public void testInterruptOnMultipleThread() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/runner/flow_interrupt001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_interrupt001");
        FlowResult result = flowEngine.execute(param);
        logger.info("end");  
    }
    
    @Test
    public void testInterruptOnSingleThread() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/runner/flow_interrupt002.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_interrupt002");
        FlowResult result = flowEngine.execute(param);
        logger.info("end");  
    }
}
