package com.jd.easyflow.flow.model;

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
    
}
