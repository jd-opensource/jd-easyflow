package com.jd.easyflow.fsm.event;

import com.jd.easyflow.fsm.model.FsmLifeCycle;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FsmEventListener extends FsmLifeCycle {
	
	/**
	 * Get accepted events.
	 * @return
	 */
	default Pair<String, Integer>[] getAcceptedEvents() {
		return null;
	}
	
	/**
	 * Get listener id.
	 * @return
	 */
	default String getId() {
	    return this.getClass().getName();
	}

	/**
	 * Event process.
	 * @param event
	 */
    public void on(FsmEvent event);

}
