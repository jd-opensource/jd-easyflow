package com.jd.easyflow.flow.filter;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public class ExpFilter<T, R> implements Filter<T, R>{
    
    private static final Logger logger = LoggerFactory.getLogger(ExpFilter.class);
    
    private String exp;
    
    public ExpFilter() {
        
    }
    
    public ExpFilter(String exp) {
        this.exp = exp;
    }

    @Override
    public R doFilter(T request, FilterChain<T, R> chain) {
        Map<String, Object> data =  new HashMap<>();
        data.put("request", request);
        data.put("chain", chain);
        if (logger.isDebugEnabled()) {
            logger.debug("EVAL SPEL:" + exp);
        }
        R result = ElFactory.get().evalWithDefaultContext(exp, data, true);
        if (logger.isDebugEnabled()) {
            logger.debug("SPEL RESULT:" + JsonUtil.toJsonString(result));
        }
        return result;
    }

}
