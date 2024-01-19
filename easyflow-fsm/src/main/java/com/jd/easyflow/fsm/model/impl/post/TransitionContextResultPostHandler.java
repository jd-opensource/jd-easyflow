package com.jd.easyflow.fsm.model.impl.post;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 */
public class TransitionContextResultPostHandler extends AbstractTransitionPostHandler {

    @Override
    public PostHandleResult postHandle(TransitionContext transitionContext, FsmContext context) {
        return new PostHandleResult(transitionContext.getPostStateId(), transitionContext.getPostEventId());
    }

}
