package com.jd.easyflow.flow.cases.performance;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.SpelEvaluator;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.SpelHelper;

/**
 * 
 * @author liyuliang5
 *
 */
public class PerformanceTest {

    private static final Logger log = LoggerFactory.getLogger(PerformanceTest.class);

    /**
     * test one node exeucte multiple times
     */
    @Test
    public void test1() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/performance/flow_performance001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_performance001", "EMPTY_NODE2", null);
        FlowResult result = flowEngine.execute(param);

        // start test
        log.info("start test");
        long t1 = System.currentTimeMillis();
        int count = 10000;
        for (int i = 0; i < count; i++) {
            param = new FlowParam("flow_performance001", "EMPTY_NODE2", null);
            result = flowEngine.execute(param);
        }
        long time = System.currentTimeMillis() - t1;
        log.info("execute " + count + "times, elapse " + time + "ms");
    }

    /**
     * test one flow with one SPEL node 
     */
    @Test
    public void test2() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/performance/flow_performance001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_performance001", "EMPTY_NODE", null);
        FlowResult result = flowEngine.execute(param);

        // start test
        log.info("start test");
        long t1 = System.currentTimeMillis();
        int count = 10000;
        for (int i = 0; i < count; i++) {
            param = new FlowParam("flow_performance001", "EMPTY_NODE", null);
            result = flowEngine.execute(param);
        }
        long time = System.currentTimeMillis() - t1;
        log.info("execute " + count + " times, elapse" + time + "ms");
    }

    /**
     * test one flow execute multiple times
     */
    @Test
    public void test3() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/performance/flow_performance001.json");
        flowEngine.init();
        int count = 10000;
        FlowParam param = new FlowParam("flow_performance001", "EMPTY_NODE3", null);
        param.put("count", 1);
        FlowResult result = flowEngine.execute(param);

        // start test
        log.info("start test");
        long t1 = System.currentTimeMillis();
        param = new FlowParam("flow_performance001", "EMPTY_NODE3", null);
        param.put("count", count);
        result = flowEngine.execute(param);
        long time = System.currentTimeMillis() - t1;
        log.info("execute " + count + " times, elpase " + time + "ms");
    }
    
//    @Test
//    public void testBuildSpelRoot() {
//        NodeContext nodeContext = new NodeContext();
//        FlowContext context = new FlowContextImpl();
//        FlowParam param = new FlowParam();
//        FlowResult result = new FlowResult();
//        context.setParam(param);
//        context.setResult(result);
//        Map<String, Object> data = new HashMap<>();
//        data.put("node", "node");
//        
//        SpelEvaluator evaluator = new SpelEvaluator();
//        long t1 = System.currentTimeMillis();
//        for (int i = 0; i < 1000000; i++) {
//            evaluator.buildHashMapRoot(nodeContext, context, data);
//        }
//        long t2 = System.currentTimeMillis();
//        for (int i = 0; i < 1000000; i++) {
//            evaluator.buildRootMapRoot(nodeContext, context, data);
//        }  
//        long t3 = System.currentTimeMillis();
//        log.info((t2 - t1) + " " + (t3 - t2));
//    }
//    
//    @Test
//    public void testExecuteSpel() {
//        NodeContext nodeContext = new NodeContext();
//        FlowContext context = new FlowContextImpl();
//        FlowParam param = new FlowParam();
//        FlowResult result = new FlowResult();
//        context.setParam(param);
//        context.setResult(result);
//        Map<String, Object> data = new HashMap<>();
//        data.put("node", "node");
//        
//        SpelEvaluator evaluator = new SpelEvaluator();
//        Object root1 = evaluator.buildHashMapRoot(nodeContext, context, data);
//        Object root2 = evaluator.buildRootMapRoot(nodeContext, context, data);
//        SpelHelper.evalWithDefaultContext("context.param", root1, true);
//        SpelHelper.evalWithDefaultContext("context.param", root2, true);
//        long t1 = System.currentTimeMillis();
//        for (int i = 0; i < 1000000; i++) {
//            SpelHelper.evalWithDefaultContext("context.param", root1, true);
//        }
//        long t2 = System.currentTimeMillis();
//        for (int i = 0; i < 1000000; i++) {
//            SpelHelper.evalWithDefaultContext("context.param", root1, true);
//        }  
//        long t3 = System.currentTimeMillis();
//        for (int i = 0; i < 1000000; i++) {
//            SpelHelper.evalWithDefaultContext("context.param", root1, true);
//        }  
//        long t4 = System.currentTimeMillis();
//        log.info((t2 - t1) + " " + (t3 - t2) + " " + (t4 - t3));
//    }

}
