package com.jd.easyflow.flow.common;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 */
public interface FlowExecutor<T> {
    
    /**
     * Common execute method
     * @param context
     * @return
     */
	T execute(FlowContext context);
}
