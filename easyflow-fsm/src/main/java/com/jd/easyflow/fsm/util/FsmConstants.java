package com.jd.easyflow.fsm.util;

import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.impl.EventImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmConstants {
    
    public static final String NS_SEP = ":";
    
    public static final String NS_EASYFLOW = "easyflow";
    
    public static final String EASYFLOW_NS_PREFIX = NS_EASYFLOW + NS_EASYFLOW;

    public static final Event COMMON_ENTER_EVENT = new EventImpl("_state_enter", "State Enter Event");
    public static final Event COMMON_CHECK_EVENT = new EventImpl("_state_check", "State Check Event");

    public static final int EVENT_ORDER_START = 10000;
    
    /**
     * Record execute history, default is true
     */
    public static final String FSM_PROPERTY_RECORD_HISTORY = "fsm.recordHistory";
    
    public static final String CTX_PARENT_CONTEXT = "_parentContext";
    
    public static final String CTX_PARENT_TRANSITION_CONTEXT = "_parentTransitionContext";
    
    /**
     * initContextKey
     */
    public static final String INIT_CONTEXT_KEY = "initContext";
    
    public static final String PARAM_KEY_EL_EVALUATOR = "_fsm.elEvaluator";
    
    public static final String FSM_MANAGER_EVENT_DATA_KEY_PARAM = "param";
    public static final String FSM_MANAGER_EVENT_DATA_KEY_FSM_MANAGER = "fsmManager";
    public static final String FSM_MANAGER_EVENT_DATA_KEY_RESULT = "result";
    public static final String FSM_MANAGER_EVENT_DATA_KEY_EXCEPTION = "exception";

}
