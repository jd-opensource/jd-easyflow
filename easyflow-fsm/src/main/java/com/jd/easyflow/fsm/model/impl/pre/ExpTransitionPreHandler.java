package com.jd.easyflow.fsm.model.impl.pre;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPreHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpTransitionPreHandler implements TransitionPreHandler {

	private static final Logger logger = LoggerFactory.getLogger(ExpTransitionPreHandler.class);

	private String exp;

	public ExpTransitionPreHandler() {
	}

	public ExpTransitionPreHandler(String exp) {
		this.exp = exp;
	}

	@Override
	public boolean preHandle(TransitionContext transitionContext, FsmContext context) {
		boolean result = ElFactory.get().eval(exp, transitionContext, context, null);
		return result;
	}

	public String getExp() {
		return exp;
	}

	public void setExp(String exp) {
		this.exp = exp;
	}

}
