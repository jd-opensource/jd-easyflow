package com.jd.easyflow.flow.cases.listener;

import org.junit.Assert;
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
public class InterruptFlowListenerTest {
    
    private static final Logger logger = LoggerFactory.getLogger(InterruptFlowListenerTest.class);

    /**
     * Test interrupt.
     */
    @Test
    public void testInterrupt001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/listener/interrupt_listener_001.json");
        flowEngine.init();
        
        FlowParam param = new FlowParam("interrupt_listener_test_001");
        FlowResult result = flowEngine.execute(param);
        Assert.assertTrue(result.getContext().isInterrupted());
    }
    
    /**
     * Test interrupt.
     */
    @Test
    public void testInterrupt002() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/listener/interrupt_listener_002.json");
        flowEngine.init();
        
        FlowParam param = new FlowParam("interrupt_listener_test_002");
        FlowResult result = flowEngine.execute(param);
        Assert.assertTrue(result.getContext().isInterrupted());
    }
    
    /**
     * Test interrupt.
     */
    @Test
    public void testInterrupt003() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/listener/interrupt_listener_003.json");
        flowEngine.init();
        
        FlowParam param = new FlowParam("interrupt_listener_test_003");
        FlowResult result = flowEngine.execute(param);
        Assert.assertFalse(result.getContext().isInterrupted());
        
        param.setParam(true);
        result = flowEngine.execute(param);
        Assert.assertTrue(result.getContext().isInterrupted());
    }
    
}
