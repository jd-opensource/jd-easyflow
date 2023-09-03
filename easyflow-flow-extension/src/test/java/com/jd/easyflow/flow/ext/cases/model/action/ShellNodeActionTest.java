package com.jd.easyflow.flow.ext.cases.model.action;

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
public class ShellNodeActionTest {
    
    private static final Logger logger = LoggerFactory.getLogger(ShellNodeActionTest.class);

    /**
     * Test customize action.
     */
    @Test
    public void testLoop001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/model/action/shell_test_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("shell_test_001", null);
        FlowResult result = flowEngine.execute(param);
        logger.info("shell result:" + (result.getContext().getEndNodes()).get(0).getActionResult());
    }
    
}
