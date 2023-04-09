package com.jd.easyflow.fsm.util;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmUtil {

    /**
     * get state.
     * 
     * @param transitionContext
     * @param context
     */
    public State state(TransitionContext transitionContext, FsmContext context) {
        return context.getFsm().getState(transitionContext.getTransition().getFromId());
    }

    /**
     * get state property.
     * 
     * @param <T>
     * @param key
     * @param transitionContext
     * @param context
     * @return
     */
    public <T> T stateProperty(String key, TransitionContext transitionContext, FsmContext context) {
        return (T) state(transitionContext, context).getProperty(key);
    }
}
