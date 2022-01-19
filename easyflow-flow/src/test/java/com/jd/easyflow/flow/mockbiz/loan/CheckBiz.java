package com.jd.easyflow.flow.mockbiz.loan;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
* 
* @author liyuliang5
* @version 1.0
* @since 1.0
*/

public class CheckBiz {
    
    private static final Logger logger = LoggerFactory.getLogger(CheckBiz.class);

    public void paramCheck(FlowContext context, NodeContext nodeContext) {
        logger.info("paramCheck");
    }
    
    public void authCheck(FlowContext context) {
        logger.info("authCheck");
    }
}
