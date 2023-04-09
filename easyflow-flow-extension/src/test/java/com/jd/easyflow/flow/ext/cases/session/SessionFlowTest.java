package com.jd.easyflow.flow.ext.cases.session;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.ext.session.FlowSession;
import com.jd.easyflow.flow.ext.session.FlowSessionConstants;
import com.jd.easyflow.flow.ext.session.FlowSessionImpl;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class SessionFlowTest {

    public static final Logger logger = LoggerFactory.getLogger(SessionFlowTest.class);

    @Test
    public void testEventFlowMultipleTimes() throws Exception {   
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/session/flow_session_001.json");
        flowEngine.init();

        FlowSession session = new FlowSessionImpl();
        
        // start request
        logger.info("==============start request");
        FlowParam param = new FlowParam("flow_session_001", "NODE1", "request1");
        param.putContextData(FlowSessionConstants.CONTEXT_SESSION_KEY, session);
        FlowResult result = flowEngine.execute(param);
        // receieve node1 result
        logger.info("==============receieve node1 result");
        param = new FlowParam("flow_session_001", "NODE1", "request1 node1 result");
        param.put(FlowConstants.PARAM_DATA_EVENT, "RESULT");
        param.putContextData(FlowSessionConstants.CONTEXT_SESSION_KEY, session);
        result = flowEngine.execute(param);
        // receieve node2 result
        logger.info("==============receieve node2 result");
        param = new FlowParam("flow_session_001", "NODE2", "request1 node2 result");
        param.put(FlowConstants.PARAM_DATA_EVENT, "RESULT");
        param.putContextData(FlowSessionConstants.CONTEXT_SESSION_KEY, session);
        result = flowEngine.execute(param);

        // receieve node3 result
        logger.info("==============receieve node3 result");        
        param = new FlowParam("flow_session_001", "NODE3", "request1 node3 result");
        param.put(FlowConstants.PARAM_DATA_EVENT, "RESULT");
        param.putContextData(FlowSessionConstants.CONTEXT_SESSION_KEY, session);
        result = flowEngine.execute(param);

        // receieve node4 result
        logger.info("==============receieve node4 result");                
        param = new FlowParam("flow_session_001", "NODE4", "request1 node4 result");
        param.put(FlowConstants.PARAM_DATA_EVENT, "RESULT");
        param.putContextData(FlowSessionConstants.CONTEXT_SESSION_KEY, session);
        result = flowEngine.execute(param);
    }
    
}
