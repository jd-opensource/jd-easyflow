package com.jd.easyflow.flow.ext.cases.timeout;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 */
public class TestTimeoutNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(TestTimeoutNodeAction.class);

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            logger.error("thread interrupted");
            throw new RuntimeException(e);
        }
        return null;
    }
    
    public Object onTimeout(Map<String, Object> config) {
        logger.info("timeout!, config:{}", config);
        return "timeout!";
    }

}
