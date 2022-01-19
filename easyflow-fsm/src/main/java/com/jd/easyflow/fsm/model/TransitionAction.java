package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface TransitionAction {

    /**
     * Execute transition.
     * @param transitionContext
     * @param context
     * @return
     */
	Object execute(TransitionContext transitionContext, FsmContext context);

}
