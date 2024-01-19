package com.jd.easyflow.fsm.util;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.TransitionImpl;

/**
 * 
 * @author liyuliang5
 */
public class TransitionUtil {

    private static FixPostTransitionImpl INSTANCE = new FixPostTransitionImpl();

    /**
     * Used for post manually.
     * @param postStateId
     * @param postEventId
     * @param transitionContext
     * @param fsmContext
     */
    public static void post(String postStateId, String postEventId, TransitionContext transitionContext,
            FsmContext fsmContext) {
        INSTANCE.post(postStateId, postEventId, transitionContext, fsmContext);
    }

    private static class FixPostTransitionImpl extends TransitionImpl {

        {
            postHandler = new TransitionPostHandler() {

                @Override
                public PostHandleResult postHandle(TransitionContext transitionContext, FsmContext context) {
                    return new PostHandleResult(transitionContext.getPostStateId(), transitionContext.getPostEventId());
                }
            };
        }

        public void post(String postStateId, String postEventId, TransitionContext transitionContext,
                FsmContext fsmContext) {
            transitionContext.setPostStateId(postStateId);
            transitionContext.setPostEventId(postEventId);
            super.executePostHandler(transitionContext, fsmContext);
        }

    }
}
