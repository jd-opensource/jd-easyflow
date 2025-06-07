package com.jd.easyflow.flow.filter;

import java.util.List;
import java.util.function.Function;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public class FilterChainImpl<T, R> implements FilterChain<T, R> {

    private List<Filter<T, R>> filters;

    private Function<T, R> invoker;

    private int pos = -1;

    public FilterChainImpl(List<Filter<T, R>> filters, Function<T, R> invoker) {
        this.filters = filters;
        this.invoker = invoker;
    }

    public R doFilter(T param) {
        pos++;
        if (pos < filters.size()) {
            return filters.get(pos).doFilter(param, this);
        }
        return invoker.apply(param);
    }
}
