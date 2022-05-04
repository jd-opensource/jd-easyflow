package com.jd.easyflow.flow.bpmn.cases.chain;

import static org.junit.Assert.assertEquals;

import java.util.function.Function;

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
        flowEngine.setFlowPath("classpath:flow/cases/chain/flow_chaintest1.bpmn");
        flowEngine.init();
        
        ChainInvoker invoker = new ChainInvoker();
        invoker.setFlowEngine(flowEngine);
        
        // Execute flow instance1.
        Function<Object, Object> targetAction = (o) -> {
            return new ChainTestService().execute();
        };
        Object result = invoker.invoke("flow_chaintest1", null, targetAction);
        logger.info("Execute result:" + result);
    }
    
    @Test
    public void testChain2() {
        // Init flow engine.
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/chain/flow_chaintest2.json");
        flowEngine.init();
        
        ChainInvoker invoker = new ChainInvoker();
        invoker.setFlowEngine(flowEngine);
        
        Function<Object, Object> targetAction = (o) -> {
            return new ChainTestService().execute();
        };
        Object result = invoker.invoke("flow_chaintest2", null, targetAction);
        logger.info("Result:" + result);
        assertEquals("abc", result);
    }
}
