package com.jd.easyflow.fsm.filter;

import com.jd.easyflow.fsm.model.FsmLifeCycle;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public interface Filter<T,R> extends FsmLifeCycle {
    
    /**
     * Do filter.
     * @param request
     * @param chain
     * @return
     */
    public R doFilter(T request, FilterChain<T, R> chain);
    

}
