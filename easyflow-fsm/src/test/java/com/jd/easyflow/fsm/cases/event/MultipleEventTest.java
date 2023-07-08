package com.jd.easyflow.fsm.cases.event;

import org.junit.Assert;
import org.junit.Test;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.model.Transition;

/**
 * Test multiple event.
 * @author liyuliang5
 *
 */
public class MultipleEventTest {

    /**
     * Test multiple event parse
     */
    @Test
    public void testMultipleEvent() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/cases/multipleevent/fsm_multipleevent.json");
        manager.init();

        Fsm fsm = manager.getFsm("multiple_event");
        Assert.assertEquals(2, fsm.getTransitionList().size());
        Transition trans1 = fsm.getTransitionList().get(0);
        Transition trans2 = fsm.getTransitionList().get(1);
        Assert.assertEquals("_state_enter", trans1.getEventId());
        Assert.assertEquals("_state_check", trans2.getEventId());
        
        FsmParam param1 = new FsmParam();
        param1.setFsmId("multiple_event");
        param1.setEventId("_state_enter");
        param1.setCurrentStateId("WHITELIST");
        FsmResult result1 = fsm.run(param1);
        Assert.assertEquals(true, result1.getLastTransitionResult());
        
        FsmParam param2 = new FsmParam();
        param2.setFsmId("multiple_event");
        param2.setEventId("_state_enter");
        param2.setCurrentStateId("WHITELIST");
        FsmResult result2 = fsm.run(param1);
        Assert.assertEquals(true, result2.getLastTransitionResult());
    }
}
