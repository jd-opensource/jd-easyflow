package com.jd.easyflow.fsm.el;

import java.util.Map;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ElEvaluator {
    
    /**
     * Evaluate value using default context.
     * @param <T>
     * @param exp
     * @param root
     * @param cache
     * @return
     */
    <T> T evalWithDefaultContext(String exp, Object root, boolean cache);
    
    /**
     * Evaluate EL
     * @param <T>
     * @param exp
     * @param transitionContext
     * @param fsmContext
     * @param data
     * @return
     */
    <T> T eval(String exp, TransitionContext transitionContext, FsmContext fsmContext, Map<String, Object> data);
}
