package com.jd.easyflow.flow.model;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowPreHandler extends FlowLifeCycle {

    boolean preHandle(FlowContext context);
    
}
