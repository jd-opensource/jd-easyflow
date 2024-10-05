package com.jd.easyflow.fsm.cases.post;

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
public class PostTest {

    public static final Logger logger = LoggerFactory.getLogger(PostTest.class);

    @Test
    public void testPostManually() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/post/fsm_post_manually.json");
        manager.init();
        Fsm fsm = manager.getFsm("post_manually");
        //Empty param, default is create instance, execute from start state.
        FsmParam param = new FsmParam();
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        Assert.assertEquals("S2", result.getState().getId());
    }
}
