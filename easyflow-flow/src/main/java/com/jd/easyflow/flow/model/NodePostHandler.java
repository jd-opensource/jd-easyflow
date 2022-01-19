package com.jd.easyflow.flow.model;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * Node post handler.
 * @author liyuliang5
 * @date 2021/07/09
 */
public interface NodePostHandler {
    
    /**
     * Post handle.
     * @param nodeContext
     * @param context
     * @return
     */
    public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context);
    
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
    default <T> T getProperty(String key) {
        return null;
    }
}
