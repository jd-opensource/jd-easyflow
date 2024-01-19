package com.jd.easyflow.fsm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmContext {
    
    private State currentState;

    private State previousState;

    private Event currentEvent;

    private Event previousEvent;

    private Object stateInstance;

    private String stateInstanceId;

    private Fsm fsm;

    FsmParam param;

    FsmResult result;
    
    private boolean transitionExecuted = false;

    private boolean firstTransition = true;
    

    private State firstTransitionState;

    private Event firstTransitionEvent;

    private State firstTransitionPostState;

    private Object firstTransitionActionResult;
    
    /**
     * State of last transition
     */
    private State transitionState;
    /**
     * Event of last transition
     */
    private Event transitionEvent;
    /**
     * Last transition post state.
     */
    private State transitionPostState;
    /**
     * Last transition action result.
     */
    private Object transitionActionResult;

    private Map<String, Object> data = new HashMap<String, Object>();
    /**
     * Transition list to record history.
     */
    private List<TransitionContext> transitionList;
    /**
     * Fsm interrupted flag.
     */
    private volatile boolean interrupted = false;
    
    private Boolean preResult;
    
    private Object context;

    public Object getStateInstance() {
        return stateInstance;
    }

    public void setStateInstance(Object stateInstance) {
        this.stateInstance = stateInstance;
    }

    public String getStateInstanceId() {
        return stateInstanceId;
    }

    public void setStateInstanceId(String stateInstanceId) {
        this.stateInstanceId = stateInstanceId;
    }

    public State getCurrentState() {
        return currentState;
    }

    public void setCurrentState(State currentState) {
        this.currentState = currentState;
    }

    public Event getCurrentEvent() {
        return currentEvent;
    }

    public void setCurrentEvent(Event currentEvent) {
        this.currentEvent = currentEvent;
    }

    public Fsm getFsm() {
        return fsm;
    }

    public void setFsm(Fsm fsm) {
        this.fsm = fsm;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    public <T> T getData(String key) {
        return (T) data.get(key);
    }

    public void putData(String key, Object value) {
        data.put(key, value);
    }

    public FsmParam getParam() {
        return param;
    }

    public void setParam(FsmParam param) {
        this.param = param;
    }

    public FsmResult getResult() {
        return result;
    }

    public void setResult(FsmResult result) {
        this.result = result;
    }

    public boolean isFirstTransition() {
        return firstTransition;
    }

    public void setFirstTransition(boolean firstTransition) {
        this.firstTransition = firstTransition;
    }

    public Object getTransitionActionResult() {
        return transitionActionResult;
    }

    public void setTransitionActionResult(Object transitionActionResult) {
        this.transitionActionResult = transitionActionResult;
    }

    public State getPreviousState() {
        return previousState;
    }

    public void setPreviousState(State previousState) {
        this.previousState = previousState;
    }

    public Event getPreviousEvent() {
        return previousEvent;
    }

    public void setPreviousEvent(Event previousEvent) {
        this.previousEvent = previousEvent;
    }

    public Object getFirstTransitionActionResult() {
        return firstTransitionActionResult;
    }

    public void setFirstTransitionActionResult(Object firstTransitionActionResult) {
        this.firstTransitionActionResult = firstTransitionActionResult;
    }

    public State getFirstTransitionPostState() {
        return firstTransitionPostState;
    }

    public void setFirstTransitionPostState(State firstTransitionPostState) {
        this.firstTransitionPostState = firstTransitionPostState;
    }

    public State getTransitionPostState() {
        return transitionPostState;
    }

    public void setTransitionPostState(State transitionPostState) {
        this.transitionPostState = transitionPostState;
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

    public State getTransitionState() {
        return transitionState;
    }

    public void setTransitionState(State transitionState) {
        this.transitionState = transitionState;
    }

    public Event getTransitionEvent() {
        return transitionEvent;
    }

    public void setTransitionEvent(Event transitionEvent) {
        this.transitionEvent = transitionEvent;
    }

    public boolean isTransitionExecuted() {
        return transitionExecuted;
    }

    public void setTransitionExecuted(boolean transitionExecuted) {
        this.transitionExecuted = transitionExecuted;
    }
    
    public void addTransition(TransitionContext transition) {
        if (this.transitionList == null) {
            this.transitionList = new ArrayList();
        }
        this.transitionList.add(transition);
    }

    public List<TransitionContext> getTransitionList() {
        return transitionList;
    }

    public void setTransitionList(List<TransitionContext> transitionList) {
        this.transitionList = transitionList;
    }

    public boolean isInterrupted() {
        return interrupted;
    }

    public void setInterrupted() {
        this.interrupted = true;
    }

    public Boolean getPreResult() {
        return preResult;
    }

    public void setPreResult(Boolean preResult) {
        this.preResult = preResult;
    }

    public <T>T getContext() {
        return (T) context;
    }

    public void setContext(Object context) {
        this.context = context;
    }
    
    

}
