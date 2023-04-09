package com.jd.easyflow.flow.ext.cases.session;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class MockRequestAsyncCall {

    private static final Logger logger = LoggerFactory.getLogger(MockRequestAsyncCall.class);

    public void call(String data) {
        logger.info("call with data:" + data);
    }

    public void resultProcess(String data, NodeContext nodeContext, FlowContext context) {
        logger.info("result with data:" + data);
        Boolean end = context.getFlow().getNode(nodeContext.getNodeId()).getProperty("end");
        if (Boolean.TRUE.equals(end)) {
            logger.info("flow end");
        }
    }
}
