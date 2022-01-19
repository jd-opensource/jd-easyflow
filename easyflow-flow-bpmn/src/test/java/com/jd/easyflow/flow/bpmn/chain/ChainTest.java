package com.jd.easyflow.flow.bpmn.chain;

import static org.junit.Assert.assertEquals;

import java.util.concurrent.Callable;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.ext.chain.ChainInvoker;

public class ChainTest {
    
    private static final Logger logger = LoggerFactory.getLogger(ChainTest.class);

    /**
     * Test orchestrate plugins by nodes. 
     */
    @Test
    public void testChain1() {
        // Init flow engine.
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/chain/flow_chaintest1.bpmn");
        flowEngine.init();
        
        ChainInvoker invoker = new ChainInvoker();
        invoker.setFlowEngine(flowEngine);
        
        // Execute flow instance1.
        Callable targetAction = () -> {
            return new ChainTestService().execute();
        };
        Object result = invoker.invoke("flow_chaintest1", targetAction);
        logger.info("Execute result:" + result);
    }
    
    @Test
    public void testChain2() {
        // Init flow engine.
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/chain/flow_chaintest2.json");
        flowEngine.init();
        
        ChainInvoker invoker = new ChainInvoker();
        invoker.setFlowEngine(flowEngine);
        
        Callable targetAction = () -> {
            return new ChainTestService().execute();
        };
        Object result = invoker.invoke("flow_chaintest2", targetAction);
        logger.info("Result:" + result);
        assertEquals("abc", result);
    }
}
