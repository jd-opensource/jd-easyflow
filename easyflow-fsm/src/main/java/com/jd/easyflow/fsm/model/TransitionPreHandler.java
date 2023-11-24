package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface TransitionPreHandler extends FsmLifeCycle {

    /**
     * Pre handle.
     * @param transitionContext
     * @param context
     * @return
     */
	boolean preHandle(TransitionContext transitionContext, FsmContext context);
	
}
