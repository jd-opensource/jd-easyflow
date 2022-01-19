package com.jd.easyflow.fsm.model;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 */
public interface TransitionExecutor<T> {
    
    /**
     * Execute transition.
     * @param transitionContext
     * @param fsmContext
     * @return
     */
    T execute(TransitionContext transitionContext, FsmContext fsmContext);
}

