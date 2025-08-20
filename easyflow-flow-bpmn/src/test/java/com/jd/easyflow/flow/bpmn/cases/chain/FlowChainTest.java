package com.jd.easyflow.flow.bpmn.cases.chain;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.ext.chain.BaseChainPlugin;
import com.jd.easyflow.flow.ext.chain.ChainInvoker;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 *
 */
public class FlowChainTest {

    
    private static final Logger log = LoggerFactory.getLogger(FlowChainTest.class);

    /**
     */
    @Test
    public void testNormal() {
        // Init flow engine.
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/chain/flow_chaintest1.bpmn");
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.init();
        
        ChainInvoker chainInvoker = new ChainInvoker();
        chainInvoker.setFlowEngine(flowEngine);
        
        Function<Object, Object> invoker = (t) -> {
            log.info("Execute invoker, Param:" + t);
            Map<String, Object> result = new HashMap<>();
            result.put("output", 456);
            return result;
        };
        
        Map<String, Object> param = new HashMap<>();
        param.put("input", 123);
        
        Object result = chainInvoker.invoke("flow_chaintest1", param, invoker);
        log.info("Result:" + result);
    }
    

    public static class TestPlugin extends BaseChainPlugin {
        
        private static final Logger logger = LoggerFactory.getLogger(TestPlugin.class);
        
        private String name;
        
        public TestPlugin(String name) {
            this.name = name;
        }
        

        @Override
        public boolean preHandle(NodeContext nodeContext, FlowContext context) {
            logger.info(name + " pre handle");
            return true;
        }

        @Override
        public void postHandleNormal(NodeContext nodeContext, FlowContext context) {
            logger.info(name + " post handle normal");
        }

        @Override
        public void postHandleException(Throwable t, NodeContext nodeContext, FlowContext context) {
            logger.info(name + " post handle exception");
        }

    }
  


}
