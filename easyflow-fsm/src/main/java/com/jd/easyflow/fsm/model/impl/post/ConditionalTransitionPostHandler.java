package com.jd.easyflow.fsm.model.impl.post;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionExecutor;

/**
 * 
 * @author liyuliang5
 *
 */
public class ConditionalTransitionPostHandler  extends AbstractTransitionPostHandler {

    private static final Logger logger = LoggerFactory.getLogger(ConditionalTransitionPostHandler.class);

    private List<Map<String, Object>> branchList;

    public ConditionalTransitionPostHandler() {
    }

    public ConditionalTransitionPostHandler(Map<String, Object> branch) {
        this.branchList = Arrays.asList(branch);
    }

    public ConditionalTransitionPostHandler(List<Map<String, Object>> branchList) {
        this.branchList = branchList;
    }

    @Override
    public PostHandleResult postHandle(TransitionContext transitionContext, FsmContext context) {
        for (Map<String, Object> branch : branchList) {
            boolean result = evalCondition(branch.get("when"), transitionContext, context);
            if (result) {
                Object next = branch.get("to");
                return super.parseTo(next, transitionContext, context);
            }
        }
        return null;
    }

    private boolean evalCondition(Object condition, TransitionContext transitionContext, FsmContext context) {
        if (condition == null) {
            return true;
        }
        if (condition instanceof String) {
            return ElFactory.get().eval((String) condition, transitionContext, context, null);
        }
        return ((TransitionExecutor<Boolean>) condition).execute(transitionContext, context);
    }

}