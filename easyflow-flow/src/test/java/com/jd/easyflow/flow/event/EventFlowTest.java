package com.jd.easyflow.flow.event;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class EventFlowTest {

    public static final Logger logger = LoggerFactory.getLogger(EventFlowTest.class);

    @Test
    public void testEvent1() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/event/flow_event_001.json");
        flowEngine.init();
        
        FlowParam param = new FlowParam("flow_event_001", "EMPTY_NODE", null);
        param.put(FlowConstants.PARAM_DATA_EVENT, "EVENT1");
        FlowResult result = flowEngine.execute(param);
        logger.info("Result:" + result);
        assertEquals("EMPTY_NODE", result.getContext().getEndNodes().get(0).getNodeId());
    }
    
    @Test
    public void testEvent2() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/event/flow_event_001.json");
        flowEngine.init();
        
        FlowParam param = new FlowParam("flow_event_001", "EMPTY_NODE", null);
        param.put(FlowConstants.PARAM_DATA_EVENT, "EVENT2");
        FlowResult result = flowEngine.execute(param);
        logger.info("Result:" + result);
        assertEquals("EMPTY_NODE", result.getContext().getEndNodes().get(0).getNodeId());
    }
    
    @Test
    public void testEvent3() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/event/flow_event_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_event_001", "EMPTY_NODE", null);
        param.put(FlowConstants.PARAM_DATA_EVENT, "EVENT3");
        FlowResult result = flowEngine.execute(param);
        logger.info("Result:" + result);
        assertEquals("EMPTY_NODE2", result.getContext().getEndNodes().get(0).getNodeId());
    }
}
