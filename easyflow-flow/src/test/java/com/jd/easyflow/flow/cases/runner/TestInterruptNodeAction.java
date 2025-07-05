package com.jd.easyflow.flow.cases.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TestInterruptNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(TestInterruptNodeAction.class);

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        logger.info("start execute node:" + nodeContext.getNodeId());
        try {
            Thread.sleep(300);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        boolean interrupt = Boolean.TRUE.equals(context.getFlow().getNode(nodeContext.getNodeId()).getProperty("interrupt"));
        if (interrupt) {
            logger.info("interrupt");
            context.setInterrupted();;
        }
        logger.info("end execute node:" + nodeContext.getNodeId());
        return null;
    }

}
