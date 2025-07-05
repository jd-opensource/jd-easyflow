package com.jd.easyflow.flow.filter;

import com.jd.easyflow.flow.model.FlowLifeCycle;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public interface Filter<T,R> extends FlowLifeCycle {

    /**
     * Do filter.
     * @param request
     * @param chain
     * @return
     */
    R doFilter(T request, FilterChain<T, R> chain);
    
    /**
     * 
     * The highest order value has the highest priority. 
     * If order value same, the front configured has higher priority.
     * DO NOT override this method unless you have requirement and know its behavior.
     * @return
     */
    default int getOrder() {return 0;};
    
}
