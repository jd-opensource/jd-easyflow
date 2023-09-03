package com.jd.easyflow.fsm.util;

import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.impl.EventImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmConstants {

    public static final Event COMMON_ENTER_EVENT = new EventImpl("_state_enter", "State Enter Event");
    public static final Event COMMON_CHECK_EVENT = new EventImpl("_state_check", "State Check Event");

    public static final int EVENT_ORDER_START = 10000;
    
    /**
     * Record execute history， default is true
     */
    public static final String FSM_PROPERTY_RECORD_HISTORY = "fsm.recordHistory";
    
    public static final String CTX_PARENT_CONTEXT = "parentContext";
    
    public static final String CTX_PARENT_TRANSITION_CONTEXT = "parentTransitionContext";

}
