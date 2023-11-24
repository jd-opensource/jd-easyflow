package com.jd.easyflow.flow.model;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public interface FlowLifeCycle {

    /**
     * Invoked after element constructed on flow parsing, the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     * @param definition
     * @param context
     */
    default void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}

    /**
     * Invoked after Flow is constructed,  the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     * @param initContext
     * @param parent
     */
    default void init(InitContext initContext, Object parent) {}
    
    /**
     * Invoked on FlowEngine destroy,  the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     */
    default void destroy() {}
}
