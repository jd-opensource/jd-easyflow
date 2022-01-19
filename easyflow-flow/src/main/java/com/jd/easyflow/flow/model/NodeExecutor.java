package com.jd.easyflow.flow.model;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * Executor of node and subcomponent level.
 * @author liyuliang5
 *
 * @param <T>
 */
public interface NodeExecutor<T> {

    /**
     * Execute.
     * @param nodeContext
     * @param context
     * @return
     */
	public T execute(NodeContext nodeContext, FlowContext context);
}
