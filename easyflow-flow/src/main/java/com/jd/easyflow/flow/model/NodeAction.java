package com.jd.easyflow.flow.model;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface NodeAction {
    
    /**
     * Execute node action.
     * @param <T>
     * @param nodeContext
     * @param context
     * @return
     */
    <T>T execute(NodeContext nodeContext, FlowContext context);
    
    /**
     * Init node action.
     * @param initContext
     * @param node
     */
    default void init(InitContext initContext, FlowNode node) {
    }
    
    /**
     * Get node action properties.
     * @return
     */
    default Map<String, Object> getProperties() {
        return null;
    }
    
    /**
     * Get single property value.
     * @param <T>
     * @param key
     * @return
     */
    default <T> T getProperty(String key) {
        return null;
    }

}
