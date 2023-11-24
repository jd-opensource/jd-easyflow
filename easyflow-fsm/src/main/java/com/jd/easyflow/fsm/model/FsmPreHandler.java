package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FsmPreHandler extends FsmLifeCycle {

    public boolean preHandle(FsmContext context);
    
}
