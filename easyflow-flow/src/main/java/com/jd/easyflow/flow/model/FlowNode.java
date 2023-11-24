package com.jd.easyflow.flow.model;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowNode extends FlowLifeCycle {

    /**
     * Execute node.
     * @param nodeContext
     * @param context
     * @return
     */
    NodeContext execute(NodeContext nodeContext, FlowContext context);
    
    /**
     * Get node ID
     * @return
     */
    default String getId() {
        return null;
    }

    /**
     * Get node name.
     * @return
     */
    default String getName() {
        return null;
    }

    /**
     * Get all node properties.
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
    default <T> T getProperty(String key) {
        Map<String, Object> properties = getProperties();
        if (properties == null) {
            return null;
        }
        return (T) properties.get(key);
    }
    
    /**
     * Set property.
     * @param key
     * @param value
     */
    default void setProperty(String key, Object value) {
        getProperties().put(key, value);
    }

}
