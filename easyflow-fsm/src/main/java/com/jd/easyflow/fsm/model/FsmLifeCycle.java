package com.jd.easyflow.fsm.model;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public interface FsmLifeCycle {

    /**
     * Invoked after element constructed on fsm parsing, the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     * @param definition
     * @param context
     */
    default void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}
    
    /**
     * Invoked after Fsm is constructed,  the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     * @param initContext
     * @param parent
     */
    default void init(InitContext initContext, Object parent) {}
    
    /**
     * Invoked on FsmManager destroy,  the element SHOULD BE new instance, SHOULD NOT BE singleton or reusable.
     */
    default void destroy() {}
}
