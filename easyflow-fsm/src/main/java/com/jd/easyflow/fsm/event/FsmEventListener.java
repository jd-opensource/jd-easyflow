package com.jd.easyflow.fsm.event;

import org.apache.commons.lang3.tuple.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FsmEventListener {
	
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
