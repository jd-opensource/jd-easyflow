package com.jd.easyflow.fsm.model.impl;

import java.util.List;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.TransitionPreHandler;
import com.jd.easyflow.fsm.util.FsmEventTypes;

/**
 * 
 * @author liyuliang5
 *
 */
public class TransitionImpl implements Transition {
    
    private String fromId;
    
    private String eventId;
    
    private List<String> toIdList;
    
    private TransitionPreHandler preHandler;
    
    private TransitionAction action;
    
    private TransitionPostHandler postHandler;
    
    @Override
    public TransitionContext execute(TransitionContext transitionContext, FsmContext context) {
        if (preHandler != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_PRE_START, transitionContext, context, false);
            boolean result = preHandler.preHandle(transitionContext, context);
            transitionContext.setPreResult(result);
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_PRE_END, transitionContext, context, false);
            if (!transitionContext.isPreResult()) {
                return transitionContext;
            }
        }
        Object actionResult = null;
        if (action != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_START, transitionContext, context, false);
            actionResult = action.execute(transitionContext, context);
            transitionContext.setActionResult(actionResult);
           context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_END, transitionContext, context, false);
        }
        if (context.isFirstTransition()) {
            context.setFirstTransitionActionResult(actionResult);
        }
        context.setTransitionActionResult(actionResult);
        
        String postStateId = null;
        if (postHandler != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_POST_START, transitionContext, context, false);
            postStateId = postHandler.postHandle(transitionContext, context);
            transitionContext.setPostStateId(postStateId);
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_POST_END, transitionContext, context, false);
        }
        if (context.isFirstTransition()) {
            context.setFirstTransitionPostState(context.getFsm().getState(postStateId));
        }
        context.setTransitionPostState(context.getFsm().getState(postStateId));
        if (postStateId != null) {
            context.setPreviousState(context.getCurrentState());
            context.setCurrentState(context.getFsm().getState(postStateId));
        }
        context.setPreviousEvent(context.getCurrentEvent());
        context.setCurrentEvent(null);
        return transitionContext;
    }
    
    /**
     * Execute node action.
     * @param nodeContext
     * @param context
     */
    protected void executeAction(TransitionContext transitionContext, FsmContext context) {
        List<Filter<Pair<TransitionContext, FsmContext>, Void>> filters = context.getFsm().getTransitionActionFilters();
        if (filters == null || filters.size() == 0) {
            invokeAction(transitionContext, context);
            return;
        }
        FilterChain<Pair<TransitionContext, FsmContext>, Void> chain = new FilterChain<Pair<TransitionContext, FsmContext>, Void>(
                filters, p -> {
                    invokeAction(transitionContext, context);
                    return null;
                });
        chain.doFilter(Pair.of(transitionContext, context));
    }
    
    protected void invokeAction(TransitionContext transitionContext, FsmContext context) {
        if (action != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_START, transitionContext, context, false);
            Object actionResult = action.execute(transitionContext, context);
            transitionContext.setActionResult(actionResult);
           context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_END, transitionContext, context, false);
        }
    }

    public TransitionPreHandler getPreHandler() {
        return preHandler;
    }

    public void setPreHandler(TransitionPreHandler preHandler) {
        this.preHandler = preHandler;
    }

    public TransitionAction getAction() {
        return action;
    }

    public void setAction(TransitionAction action) {
        this.action = action;
    }

    public TransitionPostHandler getPostHandler() {
        return postHandler;
    }

    public void setPostHandler(TransitionPostHandler postHandler) {
        this.postHandler = postHandler;
    }

    @Override
    public String getFromId() {
        return fromId;
    }

    public void setFromId(String fromId) {
        this.fromId = fromId;
    }

    @Override
    public String getEventId() {
        return eventId;
    }

    public void setEventId(String eventId) {
        this.eventId = eventId;
    }

    @Override
    public List<String> getToIdList() {
        return toIdList;
    }

    public void setToIdList(List<String> toIdList) {
        this.toIdList = toIdList;
    }
    
    

}
