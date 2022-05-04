package com.jd.easyflow.flow.cases.share.nodeaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TestStepAction implements NodeAction {

    private static final Logger logger = LoggerFactory.getLogger(TestStepAction.class);

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        FlowNode flowNode = context.getFlow().getNode(nodeContext.getNodeId());
        boolean exception = Boolean.TRUE.equals(flowNode.getProperty("exception"));
        if (exception) {
            logger.info("exception");
            throw new RuntimeException("exception");
        }
        int sleep = flowNode.getProperty("sleep");
        logger.info("Sleep time is:" + sleep);
        try {
            Thread.sleep(sleep * 1000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return null;
    }

}
