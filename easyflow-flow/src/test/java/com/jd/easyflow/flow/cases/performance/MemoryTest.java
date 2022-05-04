package com.jd.easyflow.flow.cases.performance;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * Test memory usage
 * @author liyuliang5
 *
 */
public class MemoryTest {
    
    private static final Logger log = LoggerFactory.getLogger(MemoryTest.class);

    /**
     * test one node exeucte multiple times.
     * If flow.recordHistory set to true, -Xmx=5M, OOM will occur.
     */
    public void test1() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/performance/flow_memory001.json");
        flowEngine.init();
        int count = 1000000000;
        // start test
        log.info("start test");
        long t1 = System.currentTimeMillis();
        FlowParam param = new FlowParam("flow_memory_001", "EMPTY_NODE3", null);
        param.put("count", count);
        FlowResult result = flowEngine.execute(param);
        long time = System.currentTimeMillis() - t1;
        log.info("execute " + count + " times, elpase " + time + "ms");
    }
}
