package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface TransitionPostHandler extends FsmLifeCycle {
    
    /**
     * Post handle.
     * @param transitionContext
     * @param context
     * @return
     */
	PostHandleResult postHandle(TransitionContext transitionContext, FsmContext context);

}
