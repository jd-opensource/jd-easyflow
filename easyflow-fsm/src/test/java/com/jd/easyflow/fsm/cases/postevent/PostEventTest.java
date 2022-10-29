package com.jd.easyflow.fsm.cases.postevent;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.cases.mockbiz.FsmMockBizTest;
import com.jd.easyflow.fsm.util.JsonUtil;

public class PostEventTest {

    public static final Logger logger = LoggerFactory.getLogger(FsmMockBizTest.class);

    /**
     * 
     * Test create fsm instance.
     *
     */
    @Test
    public void testFsm001Create() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/postevent/fsm_postevent.json");
        manager.init();

        Fsm fsm = manager.getFsm("post_event");
        
        //Empty param，default is create instance, execute from start state.
        FsmParam param = new FsmParam();
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
    }
}
