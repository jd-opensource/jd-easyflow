package com.jd.easyflow.flow.bpmn.chain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ChainTestService {
    
    private static final Logger logger = LoggerFactory.getLogger(ChainTestService.class);

    public Object execute() {
        logger.info("ChainTestService execute");
        return "abc";
    }
}
