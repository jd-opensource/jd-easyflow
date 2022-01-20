package com.jd.easyflow.flow.common;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 * @param <T> type
 */
public interface FlowExecutor<T> {

    /**
     * Common execute method
     * 
     * @param context FlowContext
     * @return result
     */
    T execute(FlowContext context);
}
