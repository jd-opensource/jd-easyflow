package com.jd.easyflow.fsm.model;

import java.util.List;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface Transition {
    
    /**
     * Get from node ID.
     * @return
     */
    String getFromId();
    
    /**
     * Get event ID.
     * @return
     */
    String getEventId();
    
    /**
     * Get to node ID.
     * @return
     */
    List<String> getToIdList();

    /**
     * Two responsibility. 
     * 1. Execute Action, save result to context
     * 2. Get next state, put to context
     * @param transitionContext
     * @param context
     * @return
     */
	TransitionContext execute(TransitionContext transitionContext, FsmContext context);
	
}
