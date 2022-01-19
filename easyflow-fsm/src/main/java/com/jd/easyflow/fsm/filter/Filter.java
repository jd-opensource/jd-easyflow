package com.jd.easyflow.fsm.filter;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public interface Filter<T,R> {
    
    /**
     * Do filter.
     * @param request
     * @param chain
     * @return
     */
    public R doFilter(T request, FilterChain<T, R> chain);
}
