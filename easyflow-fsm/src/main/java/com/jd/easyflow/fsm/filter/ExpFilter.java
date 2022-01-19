package com.jd.easyflow.fsm.filter;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.fsm.el.ElFactory;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public class ExpFilter<T,R>  implements Filter<T, R>{
    
    private String exp;
    
    public ExpFilter() {
        
    }
    
    public ExpFilter(String exp) {
        this.exp = exp;
    }

    @Override
    public R doFilter(T request, FilterChain<T, R> chain) {
        Map<String, Object> data = new HashMap<>();
        data.put("request", request);
        data.put("chain", chain);
        return ElFactory.get().eval(exp, null, null, data);
    }
}

