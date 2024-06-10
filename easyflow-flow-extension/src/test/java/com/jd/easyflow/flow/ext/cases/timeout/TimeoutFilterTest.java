package com.jd.easyflow.flow.ext.cases.timeout;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 */
public class TimeoutFilterTest {

    @Test
    public void test() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/timeout/flow_timeout_001.json");
        flowEngine.init();
        
        FlowParam flowParam = new FlowParam("timeout_test_001");
        FlowResult flowResult = flowEngine.execute(flowParam);
        Thread.sleep(2000);
        
    }
}
