package com.jd.easyflow.flow.index;

import static org.junit.Assert.assertEquals;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

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
public class FlowIndexTest {
    
    public static final Logger logger = LoggerFactory.getLogger(FlowIndexTest.class);

    @Test
    public void testFlow001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/index/flow_index001.json");
        flowEngine.init();
        
        Map<String, Object> paramData = new HashMap<>();
        paramData.put("amount", new BigDecimal(80));
        FlowParam param = new FlowParam("flow_index001", "CONTRACT_SIGN", paramData);
       
        
        FlowResult result = flowEngine.execute(param);
        logger.info("Result:" + result);
        assertEquals("DO_LOAN", result.getContext().getEndNodes().get(0).getNodeId());
    }
}
