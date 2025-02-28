package com.jd.easyflow.fsm.cases.filter;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.Triple;

public class TransitionEmptyFilter implements Filter<Triple<Transition, TransitionContext, FsmContext>, Void> {

    @Override
    public Void doFilter(Triple<Transition, TransitionContext, FsmContext> request,
            FilterChain<Triple<Transition, TransitionContext, FsmContext>, Void> chain) {
        return null;
    }

}
