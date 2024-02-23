package com.jd.easyflow.fsm.cases.fsmmanager;

import java.util.List;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.TransitionPreHandler;

/**
 * 
 * @author liyuliang5
 */
public abstract class PocTransition implements Transition {

    protected String fromId;

    protected String eventId;

    protected List<String> toIdList;

    protected TransitionPreHandler preHandler;

    protected TransitionAction action;

    protected TransitionPostHandler postHandler;

    @Override
    public void execute(TransitionContext transitionContext, FsmContext context) {
        boolean preResult = true;
        if (preHandler != null) {
            preResult = preHandler.preHandle(transitionContext, context);
            transitionContext.setPreResult(preResult);
        }
        if (!preResult) {
            return;
        }
        if (action != null) {
            Object actionResult = action.execute(transitionContext, context);
            transitionContext.setActionResult(actionResult);
        }

        if (postHandler != null) {
            PostHandleResult postHandleResult = postHandler.postHandle(transitionContext, context);
            if (postHandleResult != null) {
                transitionContext.setPostStateId(postHandleResult.getPostStateId());
                transitionContext.setPostEventId(postHandleResult.getPostEventId());
            }
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
