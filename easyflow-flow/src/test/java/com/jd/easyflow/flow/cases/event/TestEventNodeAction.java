package com.jd.easyflow.flow.cases.event;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Test event node action
 * @author liyuliang5
 *
 */
public class TestEventNodeAction implements NodeAction  {
    
    private static final Logger logger = LoggerFactory.getLogger(TestEventNodeAction.class);

    @Override
    public Map<String, Object> execute(NodeContext nodeContext, FlowContext context) {
        logger.info("Start execute event node action");
        Map<String, Object> result =  new HashMap<>();
        result.put("result1", 1);
        return result;
    }

}
