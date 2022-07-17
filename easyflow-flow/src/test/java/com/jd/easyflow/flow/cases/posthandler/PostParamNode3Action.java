package com.jd.easyflow.flow.cases.posthandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class PostParamNode3Action implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(PostParamNode3Action.class);

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        logger.info("node3 param ");
        return null;
    }

}
