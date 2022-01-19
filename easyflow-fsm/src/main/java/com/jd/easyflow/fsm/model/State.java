package com.jd.easyflow.fsm.model;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public interface State {
	
    /**
     * Get state ID.
     * @return
     */
	String getId();
	
	/**
	 * Get state name.
	 * @return
	 */
	String getName();
	
	/**
	 * Get properties.
	 * @return
	 */
	Map<String, Object> getProperties();
	
	/**
	 * Get property.
	 * @param <T>
	 * @param key
	 * @return
	 */
	<T>T getProperty(String key);
	
}
