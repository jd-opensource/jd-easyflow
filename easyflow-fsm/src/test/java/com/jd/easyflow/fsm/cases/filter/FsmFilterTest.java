package com.jd.easyflow.fsm.cases.filter;

import org.junit.Assert;
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
 */
public class FsmFilterTest {
    
    public static final Logger logger = LoggerFactory.getLogger(FsmFilterTest.class);

    @Test
    public void testTransitionPreHandlerFilter() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/filter/tst_prehandler_filter_001.json");
        manager.init();

        Fsm fsm = manager.getFsm("tst_prehandler_filter_001");
        
        FsmParam param = new FsmParam();
        param.setEventId("E1");
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("C", result.getContext().getCurrentState().getId());
    }
    
    @Test
    public void testStateTransitionPreHandlerFilter() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/filter/tst_prehandler_filter_002.json");
        manager.init();

        Fsm fsm = manager.getFsm("tst_prehandler_filter_002");
        
        FsmParam param = new FsmParam();
        param.setCurrentStateId("A1");
        param.setEventId("E1");
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("C", result.getContext().getCurrentState().getId());
        
        param.setCurrentStateId("A2");
        param.setEventId("E1");
        result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("B", result.getContext().getCurrentState().getId());
    }
    
    @Test
    public void testPreHandlerPostConstructFilter() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/filter/tst_prehandler_filter_003.json");
        manager.init();

        Fsm fsm = manager.getFsm("tst_prehandler_filter_003");
        
        FsmParam param = new FsmParam();
        param.setCurrentStateId("A1");
        param.setEventId("E1");
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("C", result.getContext().getCurrentState().getId());
        
        param.setCurrentStateId("A2");
        param.setEventId("E1");
        result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("B", result.getContext().getCurrentState().getId());
    }
    
//    /**
//     * Dead loop when filter is empty.
//     */
//    @Test
//    public void testEmptyTransitionFilter() {
//        FsmManager manager = new FsmManager();
//        manager.setFsmPath("classpath:fsm/cases/filter/tst_empty_filter_001.json");
//        manager.init();
//
//        Fsm fsm = manager.getFsm("tst_prehandler_filter_001");
//        
//        FsmParam param = new FsmParam();
//        param.setCurrentStateId("A1");
//        param.setEventId("E1");
//        FsmResult result = fsm.run(param);
//        logger.info(result.getState().getId());
//        logger.info("RESULT:" + JsonUtil.toJsonString(result));
//        
//    }
}
