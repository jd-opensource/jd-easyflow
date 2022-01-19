package com.jd.easyflow.fsm.quickstart;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.builder.FsmBuilder;
import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.impl.EventImpl;
import com.jd.easyflow.fsm.model.impl.StateImpl;
import com.jd.easyflow.fsm.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmQuickStartTest {

    private static final Logger logger = LoggerFactory.getLogger(FsmQuickStartTest.class);
    
    /**
     * Build FSM by json configuration.
     */
    @Test
    public void testConfigSample() {
        FsmManager manager = new FsmManager();
        manager.setFsmPath("classpath:fsm/quickstart/quickstart_001.json");
        manager.init();

        Fsm fsm = manager.getFsm("quickstart_001");
        FsmParam param = new FsmParam();
        param.setEventId("E1");
        FsmResult result = fsm.run(param);
        logger.info(result.getState().getId());
        logger.info("RESULT:" + JsonUtil.toJsonString(result));
        assertEquals("B", result.getState().getId());
    }
    
    /**
     * Build FSM by coding.
     */
	@Test
	public void testCodingSample() {
		// Create FSM
		State stateA = new StateImpl("A");
		State stateB = new StateImpl("B");
		Event event1 = new EventImpl("1");
		Fsm fsm = FsmBuilder.create("test").transition(stateA, event1, stateB).build();
		// Execute FSM
		FsmContext context = new FsmContext();
		context.setCurrentState(stateA);
		context.setCurrentEvent(event1);
		FsmParam param = new FsmParam();
		param.setContext(context);
		FsmResult result = fsm.run(param);
		logger.info("RESULT:" + JsonUtil.toJsonString(result));
	}
}
