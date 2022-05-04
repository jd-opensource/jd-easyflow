package com.jd.easyflow.flow.cases.mockbiz;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class LoanBiz {

    private static final Logger logger = LoggerFactory.getLogger(LoanBiz.class);
    
    public void doLoan() {
        logger.info("finish loan command");
    }
}
