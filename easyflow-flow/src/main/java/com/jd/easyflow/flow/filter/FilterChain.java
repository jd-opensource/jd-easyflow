package com.jd.easyflow.flow.filter;

/**
 * @author liyuliang5
 * @param <T>
 * @param <R>
 */
public interface FilterChain<T, R> {

    public R doFilter(T param);
}
