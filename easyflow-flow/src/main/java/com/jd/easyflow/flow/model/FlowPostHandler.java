package com.jd.easyflow.flow.model;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowPostHandler {

    void postHandle(FlowContext context);
    
    default void init(InitContext initContext, Flow flow) {

    }
}
