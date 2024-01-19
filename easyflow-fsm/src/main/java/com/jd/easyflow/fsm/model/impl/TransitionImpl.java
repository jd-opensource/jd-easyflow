package com.jd.easyflow.fsm.model.impl;

import java.util.List;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.InitContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
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

    protected String fromId;

    protected String eventId;

    protected List<String> toIdList;

    protected TransitionPreHandler preHandler;

    protected TransitionAction action;

    protected TransitionPostHandler postHandler;
    
    @Override
    public void init(InitContext initContext, Object parent) {
        if (preHandler != null) {
            preHandler.init(initContext, this);
        }
        if (action != null) {
            action.init(initContext, this);
        }
        if (postHandler != null) {
            postHandler.init(initContext, this);
        }
    }

    @Override
    public void execute(TransitionContext transitionContext, FsmContext context) {
        if (! executePreHandler(transitionContext, context)) {
            return;
        }
        executeAction(transitionContext, context);
        executePostHandler(transitionContext, context);
    }

    /**
     * Execute transition action.
     * 
     * @param transitionContext
     * @param context
     */
    protected void executeAction(TransitionContext transitionContext, FsmContext context) {
        List<Filter<Pair<TransitionContext, FsmContext>, Object>> filters = context.getFsm().getTransitionActionFilters();
        if (filters == null || filters.size() == 0) {
            invokeAction(transitionContext, context);
        } else {
            FilterChain<Pair<TransitionContext, FsmContext>, Object> chain = new FilterChain<Pair<TransitionContext, FsmContext>, Object>(
                    filters, p -> {
                        return invokeAction(transitionContext, context);
                    });
            Object result = chain.doFilter(Pair.of(transitionContext, context));
            transitionContext.setActionResult(result);
        }
        
        if (context.isFirstTransition()) {
            context.setFirstTransitionActionResult(transitionContext.getActionResult());
        }
        context.setTransitionActionResult(transitionContext.getActionResult());
    }
    
    protected Object invokeAction(TransitionContext transitionContext, FsmContext context) {
        if (action != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_START, transitionContext, context,
                    false);
            Object actionResult = action.execute(transitionContext, context);
            transitionContext.setActionResult(actionResult);
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_ACTION_END, transitionContext, context,
                    false);
        }
        return transitionContext.getActionResult();
    }
    
    /**
     * Execute transition preHandler.
     * 
     * @param transitionContext
     * @param context
     */
    protected boolean executePreHandler(TransitionContext transitionContext, FsmContext context) {
        List<Filter<Pair<TransitionContext, FsmContext>, Boolean>> filters = context.getFsm().getTransitionPreHandlerFilters();
        if (filters == null || filters.size() == 0) {
            return invokePreHandler(transitionContext, context);
        }
        FilterChain<Pair<TransitionContext, FsmContext>, Boolean> chain = new FilterChain<Pair<TransitionContext, FsmContext>, Boolean>(
                filters, p -> {
                    return invokePreHandler(transitionContext, context);
                });
        Boolean result = chain.doFilter(Pair.of(transitionContext, context));
        transitionContext.setPreResult(result);
        return result == null ? true : result;
    }
    
    protected boolean invokePreHandler(TransitionContext transitionContext, FsmContext context) {
        if (preHandler != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_PRE_START, transitionContext, context,
                    false);
            boolean result = preHandler.preHandle(transitionContext, context);
            transitionContext.setPreResult(result);
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_PRE_END, transitionContext, context,
                    false);
        }
        return transitionContext.getPreResult() == null ? true : transitionContext.getPreResult();
    }
    
    /**
     * Execute transition postHandler.
     * 
     * @param transitionContext
     * @param context
     */
    protected void executePostHandler(TransitionContext transitionContext, FsmContext context) {
        List<Filter<Pair<TransitionContext, FsmContext>, PostHandleResult>> filters = context.getFsm()
                .getTransitionPostHandlerFilters();
        PostHandleResult postHandleResult = null;
        if (filters == null || filters.size() == 0) {
            postHandleResult = invokePostHandler(transitionContext, context);
        } else {
            FilterChain<Pair<TransitionContext, FsmContext>, PostHandleResult> chain = new FilterChain<Pair<TransitionContext, FsmContext>, PostHandleResult>(
                    filters, p -> {
                        return invokePostHandler(transitionContext, context);
                    });
            postHandleResult = chain.doFilter(Pair.of(transitionContext, context));
            transitionContext.setPostStateId(postHandleResult == null ? null : postHandleResult.getPostStateId());
            transitionContext.setPostEventId(postHandleResult == null ? null : postHandleResult.getPostEventId());
        }
        
        String postStateId = postHandleResult == null ? null : postHandleResult.getPostStateId();
        String postEventId = postHandleResult == null ? null :  postHandleResult.getPostEventId();
        if (context.isFirstTransition()) {
            context.setFirstTransitionPostState(context.getFsm().getState(postStateId));
        }
        context.setTransitionPostState(context.getFsm().getState(postStateId));
        
        if (postStateId != null) {
            context.setPreviousState(context.getCurrentState());
            context.setCurrentState(context.getFsm().getState(postStateId));
        }
        context.setPreviousEvent(context.getCurrentEvent());
        context.setCurrentEvent(postEventId == null ? null : context.getFsm().getEvent(postEventId));
    }
    
    protected PostHandleResult invokePostHandler(TransitionContext transitionContext, FsmContext context) {
        String postStateId = null;
        String postEventId = null;
        if (postHandler != null) {
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_POST_START, transitionContext, context,
                    false);
            PostHandleResult postHandleResult = postHandler.postHandle(transitionContext, context);
            if (postHandleResult != null) {
                postStateId = postHandleResult.getPostStateId();
                postEventId = postHandleResult.getPostEventId();
            }
            transitionContext.setPostStateId(postStateId);
            transitionContext.setPostEventId(postEventId);
            context.getFsm().getEventTrigger().triggerEvent(FsmEventTypes.TST_POST_END, transitionContext, context,
                    false);
            postStateId = transitionContext.getPostStateId();
            postEventId = transitionContext.getPostEventId();
        }
        if (postStateId == null && postEventId == null) {
            return null;
        }
        return new PostHandleResult(postStateId, postEventId);
    }
    
    @Override
    public void destroy() {
        if (preHandler != null) {
            preHandler.destroy();
        }
        if (action != null) {
            action.destroy();
        }
        if (postHandler != null) {
            postHandler.destroy();
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
