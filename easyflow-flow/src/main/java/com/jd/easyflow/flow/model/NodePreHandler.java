package com.jd.easyflow.flow.model;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface NodePreHandler {
	
    /**
     * Pre handle.
     * @param nodeContext
     * @param context
     * @return
     */
	boolean preHandle(NodeContext nodeContext, FlowContext context);
	
	/**
	 * Init.
	 * @param initContext
	 * @param node
	 */
	default void init(InitContext initContext, FlowNode node) {
	    
	}
	
	/**
	 * Get properties.
	 * @return
	 */
    default Map<String, Object> getProperties() {
        return null;
    }

    /**
     * Get property.
     * @param <T>
     * @param key
     * @return
     */
    default <T> T getProperties(String key) {
        return null;
    }
}
