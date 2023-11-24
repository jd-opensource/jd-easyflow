package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FsmPostHandler extends FsmLifeCycle {

    public void postHandle(FsmContext context);
   
}
