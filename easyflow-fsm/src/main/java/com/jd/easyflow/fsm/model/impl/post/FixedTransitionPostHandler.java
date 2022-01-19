package com.jd.easyflow.fsm.model.impl.post;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FixedTransitionPostHandler extends AbstractTransitionPostHandler {
	
	private Object nextState;
	
	public FixedTransitionPostHandler(Object nextState) {
		this.nextState = nextState;
	}

	@Override
	public String postHandle(TransitionContext transitionContext, FsmContext context) {
		return super.parseToStateId(nextState, transitionContext, context);
	}

}
