package com.jd.easyflow.fsm.model.impl.post;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FixedTransitionPostHandler extends AbstractTransitionPostHandler {
	
	private Object to;
	
	public FixedTransitionPostHandler(Object to) {
		this.to = to;
	}

	@Override
	public PostHandleResult postHandle(TransitionContext transitionContext, FsmContext context) {
		return super.parseTo(to, transitionContext, context);
	}

}
