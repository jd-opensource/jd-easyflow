package com.jd.easyflow.fsm.model.impl.action;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpTransitionAction  implements TransitionAction {

	private static final Logger logger = LoggerFactory.getLogger(ExpTransitionAction.class);

	private String exp;

	public ExpTransitionAction() {
	}

	public ExpTransitionAction(String exp) {
		this.exp = exp;
	}

	@Override
	public Object execute(TransitionContext transitionContext, FsmContext context) {
		Object result = ElFactory.get().eval(exp, transitionContext, context, null);
		return result;
	}

	public String getExp() {
		return exp;
	}

	public void setExp(String exp) {
		this.exp = exp;
	}

}
