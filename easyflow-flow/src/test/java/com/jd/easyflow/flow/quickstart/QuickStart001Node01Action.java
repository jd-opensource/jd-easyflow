package com.jd.easyflow.flow.quickstart;

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
public class QuickStart001Node01Action implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(QuickStart001Node01Action.class);

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        logger.info("Execute Node 001");
        return null;
    }

}
