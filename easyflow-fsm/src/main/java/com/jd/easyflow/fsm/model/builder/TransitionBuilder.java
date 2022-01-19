package com.jd.easyflow.fsm.model.builder;

import java.util.List;

import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.TransitionPreHandler;
import com.jd.easyflow.fsm.model.impl.TransitionImpl;
import com.jd.easyflow.fsm.model.impl.post.FixedTransitionPostHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class TransitionBuilder {
	
	private TransitionImpl transition = new TransitionImpl();
	
	private TransitionBuilder() {
		// NOOP
	}

	public static TransitionBuilder create() {
		return new TransitionBuilder();
	}
	
	public TransitionBuilder fromId(String fromId) {
	    transition.setFromId(fromId);
	    return this;
	}
	
	public TransitionBuilder eventId(String eventId) {
	    transition.setEventId(eventId);
	    return this;
	}
	
	public TransitionBuilder toIdList(List<String> toIdList) {
	    transition.setToIdList(toIdList);
	    return this;
	}
	
	
	public TransitionBuilder preHandler(TransitionPreHandler preHandler) {
		transition.setPreHandler(preHandler);
		return this;
	}
	
	public TransitionBuilder action(TransitionAction action) {
		transition.setAction(action);
		return this;
	}
	
	public TransitionBuilder postHandler(TransitionPostHandler postHandler) {
		transition.setPostHandler(postHandler);
		return this;
	}
	
	
	public TransitionBuilder to(State state) {
	    if (state == null) {
	        return this;
	    }
		transition.setPostHandler(new FixedTransitionPostHandler(state.getId()));
		return this;
	}
	
	public Transition build() {
		return this.transition;
	}
}
