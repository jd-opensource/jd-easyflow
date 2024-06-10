package com.jd.easyflow.flow.ext.cases.timeout;

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
    
    public Object onTimeout() {
        logger.info("timeout!");
        return "timeout!";
    }

}
