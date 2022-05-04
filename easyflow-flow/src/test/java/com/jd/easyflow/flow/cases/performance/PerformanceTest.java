package com.jd.easyflow.flow.cases.performance;

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

}
