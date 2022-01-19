package com.jd.easyflow.fsm.model;

/**
 * 
 * @author liyuliang5
 *
 */
public interface Event {

    /**
     * Get event id.
     * @return
     */
	String getId();
	
	/**
	 * Get event name.
	 * @return
	 */
	String getName();
}
