package com.jd.easyflow.flow.ext.cases.chain;

import static org.junit.Assert.assertEquals;

import java.util.function.Function;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.ext.chain.ChainInvoker;

public class ChainTest {
    
    private static final Logger logger = LoggerFactory.getLogger(ChainTest.class);

    
    @Test
    public void testChain2() {
        // Init flow engine.
        FlowEngineImpl flowEngine = new FlowEngineImpl();
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
