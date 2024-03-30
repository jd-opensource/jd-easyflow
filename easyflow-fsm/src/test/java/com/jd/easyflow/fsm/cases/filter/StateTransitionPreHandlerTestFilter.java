package com.jd.easyflow.fsm.cases.filter;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 */
public class StateTransitionPreHandlerTestFilter implements Filter<Pair<TransitionContext, FsmContext>, Boolean> {
    
    private static final Logger logger = LoggerFactory.getLogger(StateTransitionPreHandlerTestFilter.class);
    
    private List<String> stateList;

    public StateTransitionPreHandlerTestFilter(Map<String, Object> def) {
        stateList = (List<String>) def.get("states");
    }

    @Override
    public Boolean doFilter(Pair<TransitionContext, FsmContext> request,
            FilterChain<Pair<TransitionContext, FsmContext>, Boolean> chain) {
        if (stateList != null) {
            if (! stateList.contains(request.getRight().getCurrentState().getId())) {
                logger.info("preHandler filter skip");
                chain.doFilter(request);
                return true;
            }
        }
        
        FsmContext context = request.getRight();
        context.setCurrentState(context.getFsm().getState("C"));
        return false;
    }

}
