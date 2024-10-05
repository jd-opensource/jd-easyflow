package com.jd.easyflow.fsm.cases.mockbiz;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmMockBizTest {

    
    public static final Logger logger = LoggerFactory.getLogger(FsmMockBizTest.class);

    /**
     * 
     * Test create fsm instance.
     *
     */
    @Test
    public void testFsm001Create() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/mockbiz/fsm_mockbiz.json");
        manager.init();

        Fsm fsm = manager.getFsm("apply_001");
        
        //Empty param, default is create instance, execute from start state.
        FsmParam param = new FsmParam();
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
    }
    
    @Test
    public void testFsm001Run() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/mockbiz/fsm_mockbiz.json");
        manager.init();

        Fsm fsm = manager.getFsm("apply_001");
        
        FsmParam param = new FsmParam();
        param.setCurrentStateId("COMPANY_AUTH");
        param.setEventId("SUBMIT_COMPANY_AUTH");
        param.setParam("Test Company");
        FsmResult result = fsm.run(param);
        logger.info("Result:" + JsonUtil.toJsonString(result));
    }
    
    @Test
    public void testConditionTrue() {
        WhitelistService.whitelist = true;
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/mockbiz/fsm_mockbiz.json");
        manager.init();

        Fsm fsm = manager.getFsm("apply_001");
        
        FsmParam param = new FsmParam();
        param.setCurrentStateId("WHITELIST");
        param.setEventId("QUERY_WHITELIST");
        param.setParam("Test Company");
        FsmResult result = fsm.run(param);
        logger.info("Result:" + JsonUtil.toJsonString(result));    
        assertEquals("COMPANY_AUTH", result.getState().getId());
    }
    
    @Test
    public void testConditionFalse() {
        WhitelistService.whitelist = false;
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/mockbiz/fsm_mockbiz.json");
        manager.init();

        Fsm fsm = manager.getFsm("apply_001");
        
        FsmParam param = new FsmParam();
        param.setCurrentStateId("WHITELIST");
        param.setEventId("QUERY_WHITELIST");
        param.setParam("Test Company");
        FsmResult result = fsm.run(param);
        logger.info("Result:" + JsonUtil.toJsonString(result));    
        assertEquals("WHITELIST", result.getState().getId());
    }
}
