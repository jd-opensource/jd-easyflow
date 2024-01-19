package com.jd.easyflow.fsm.cases.post;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.FsmUtil;
import com.jd.easyflow.fsm.util.TransitionUtil;

/**
 * 
 * @author liyuliang5
 */
public class TestPostManuallyTransitionPreHandlerFilter
        implements Filter<Pair<TransitionContext, FsmContext>, Boolean> {

    @Override
    public Boolean doFilter(Pair<TransitionContext, FsmContext> request,
            FilterChain<Pair<TransitionContext, FsmContext>, Boolean> chain) {
        TransitionContext transitionContext = request.getLeft();
        FsmContext fsmContext = request.getRight();
        State state = FsmUtil.state(transitionContext, fsmContext);
        if ("S1".equals(state.getId())) {
            TransitionUtil.post("S2", null, transitionContext, fsmContext);
            return false;

        }
        return chain.doFilter(request);
    }

}
