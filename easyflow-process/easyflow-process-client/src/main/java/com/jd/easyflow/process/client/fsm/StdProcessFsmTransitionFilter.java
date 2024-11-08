package com.jd.easyflow.process.client.fsm;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.Triple;

/**
 * 
 * @author liyuliang5
 */
public class StdProcessFsmTransitionFilter extends StdProcessFsmListener implements Filter<Triple<Transition, TransitionContext, FsmContext>, Void> {

    @Override
    public Void doFilter(Triple<Transition, TransitionContext, FsmContext> request,
            FilterChain<Triple<Transition, TransitionContext, FsmContext>, Void> chain) {
        super.onTstStart(request.getMiddle(), request.getRight());
        chain.doFilter(request);
        super.onTstEnd(request.getMiddle(), request.getRight());
        return null;
    }

}
