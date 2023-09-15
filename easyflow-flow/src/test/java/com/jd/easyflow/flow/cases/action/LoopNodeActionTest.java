package com.jd.easyflow.flow.cases.action;

import java.util.Arrays;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class LoopNodeActionTest {

    /**
     * Test customize action.
     */
    @Test
    public void testLoop001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/action/loop_test_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("loop_test_001", null);
        param.put("loopMaxCount", 3);
        param.put("data", Arrays.asList("1","2","3","4"));
        FlowResult result = flowEngine.execute(param);
    }
    
}
