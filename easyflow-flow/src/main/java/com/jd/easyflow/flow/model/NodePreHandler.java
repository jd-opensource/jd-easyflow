package com.jd.easyflow.flow.model;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface NodePreHandler extends FlowLifeCycle {
	
    /**
     * Pre handle.
     * @param nodeContext
     * @param context
     * @return
     */
	boolean preHandle(NodeContext nodeContext, FlowContext context);
	
}
