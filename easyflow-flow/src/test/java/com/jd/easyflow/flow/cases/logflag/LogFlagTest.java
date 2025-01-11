package com.jd.easyflow.flow.cases.logflag;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * @author liyuliang5
 */
public class LogFlagTest {

    private static final Logger logger = LoggerFactory.getLogger(LogFlagTest.class);

    @Test(expected = Exception.class)
    public void testLogFlag1() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/logflag/logflag_001.json");
        flowEngine.init();

        FlowParam param = new FlowParam("logFlag_001");
        param.setLogFlag(false);
        flowEngine.execute(param);

    }
    
    public void throwException() throws Exception {
        throw new Exception("testException");
    }
}
