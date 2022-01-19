package com.jd.easyflow.fsm.mockbiz;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class CompanyAuthService {
    
    public static final Logger logger = LoggerFactory.getLogger(CompanyAuthService.class);

    public boolean isAuth() {
        return false;
    }
    
    public void saveCompanyAuth(String companyName) {
        logger.info("Save company realname:" + companyName);
    }
}
