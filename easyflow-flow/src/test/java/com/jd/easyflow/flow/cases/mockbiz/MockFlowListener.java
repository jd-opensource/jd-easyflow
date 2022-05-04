package com.jd.easyflow.flow.cases.mockbiz;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.util.FlowEventTypes;

/**
 * 
 * Mock Flow Listener
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class MockFlowListener implements FlowEventListener {

    private static final Logger logger = LoggerFactory.getLogger(MockFlowListener.class);

    /**
     *
     */
    @Override
    public void on(FlowEvent flowEvent) {
        switch (flowEvent.getType()) {
        case FlowEventTypes.FLOW_START: {
            logger.info("FLOW START");
            break;
        }
        case FlowEventTypes.FLOW_COMPLETE : {
            logger.info("FLOW END");
            break;
        }
        default : {
            // NOOP
        }
        }

    }

}
