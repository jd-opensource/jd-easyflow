package com.jd.easyflow.fsm;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.State;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmResult {
    
    /**
     * Fsm instance ID
     */
    private String instanceId;
    /**
     * Fsm instance
     */
    private Object instance;
    /**
     * Fsm state
     */
    private State state;
    /**
     * Fist transition result！
     * business result.
     */
    private Object result;
    

    private boolean transitionExecuted;
    /**
     * only first transition executed.
     */
    private boolean firstTransition;

    /**
     * First transition result.
     */
    private State firstTransitionState;
    private Event firstTransitionEvent;
    private Object firstTransitionResut;
    /**
     * Last transition result.
     */
    private State lastTransitionState;
    private Event lastTransitionEvent;
    private Object lastTransitionResult;
    @JsonIgnore
    private FsmContext context;
    

    public String getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
    }

    public Object getInstance() {
        return instance;
    }

    public void setInstance(Object instance) {
        this.instance = instance;
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public FsmContext getContext() {
        return context;
    }

    public void setContext(FsmContext context) {
        this.context = context;
    }

    public Object getResult() {
        return result;
    }

    public void setResult(Object result) {
        this.result = result;
    }

    public boolean isTransitionExecuted() {
        return transitionExecuted;
    }

    public void setTransitionExecuted(boolean transitionExecuted) {
        this.transitionExecuted = transitionExecuted;
    }

    public boolean isFirstTransition() {
        return firstTransition;
    }

    public void setFirstTransition(boolean firstTransition) {
        this.firstTransition = firstTransition;
    }

    public State getFirstTransitionState() {
        return firstTransitionState;
    }

    public void setFirstTransitionState(State firstTransitionState) {
        this.firstTransitionState = firstTransitionState;
    }

    public Event getFirstTransitionEvent() {
        return firstTransitionEvent;
    }

    public void setFirstTransitionEvent(Event firstTransitionEvent) {
        this.firstTransitionEvent = firstTransitionEvent;
    }

    public Object getFirstTransitionResut() {
        return firstTransitionResut;
    }

    public void setFirstTransitionResut(Object firstTransitionResut) {
        this.firstTransitionResut = firstTransitionResut;
    }

    public State getLastTransitionState() {
        return lastTransitionState;
    }

    public void setLastTransitionState(State lastTransitionState) {
        this.lastTransitionState = lastTransitionState;
    }

    public Event getLastTransitionEvent() {
        return lastTransitionEvent;
    }

    public void setLastTransitionEvent(Event lastTransitionEvent) {
        this.lastTransitionEvent = lastTransitionEvent;
    }

    public Object getLastTransitionResult() {
        return lastTransitionResult;
    }

    public void setLastTransitionResult(Object lastTransitionResult) {
        this.lastTransitionResult = lastTransitionResult;
    }

    
    
    
}
