package com.jd.easyflow.fsm.cases.filter;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 */
public class TransitionPreHandlerTestFilter implements Filter<Pair<TransitionContext, FsmContext>, Boolean> {

    @Override
    public Boolean doFilter(Pair<TransitionContext, FsmContext> request,
            FilterChain<Pair<TransitionContext, FsmContext>, Boolean> chain) {
        FsmContext context = request.getRight();
        context.setCurrentState(context.getFsm().getState("C"));
        return false;
    }

}
