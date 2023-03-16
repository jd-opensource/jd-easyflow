package com.jd.easyflow.flow.cases.parallel;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class ParallelTest {

    /**
     * test parallel nodes.
     */
    @Test
    public void testParallel001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/parallel/flow_parallel001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_parallel001");
        FlowResult result = flowEngine.execute(param);
    }
}
