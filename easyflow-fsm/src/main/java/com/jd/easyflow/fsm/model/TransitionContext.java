package com.jd.easyflow.fsm.model;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class TransitionContext {
    /**
     * Transition definition，for history trace.
     */
    private Transition transition;
    
    private boolean preResult;
    
    private Object actionResult;
    
    private String postStateId;

    /**
     * Common Data Map;
     */
    private Map<String, Object> dataMap = new HashMap<String, Object>();
    
    private Throwable throwable;

    public Map<String, Object> getDataMap() {
        return dataMap;
    }

    public void setDataMap(Map<String, Object> dataMap) {
        this.dataMap = dataMap;
    }
    
    public void put(String key, Object value) {
        dataMap.put(key, value);
    }

    public <T> T get(String key) {
        return (T) dataMap.get(key);
    }

    public void remove(String key) {
        dataMap.remove(key);
    }

    public Throwable getThrowable() {
        return throwable;
    }

    public void setThrowable(Throwable throwable) {
        this.throwable = throwable;
    }

    public boolean isPreResult() {
        return preResult;
    }

    public void setPreResult(boolean preResult) {
        this.preResult = preResult;
    }

    public Object getActionResult() {
        return actionResult;
    }

    public void setActionResult(Object actionResult) {
        this.actionResult = actionResult;
    }

    public String getPostStateId() {
        return postStateId;
    }

    public void setPostStateId(String postStateId) {
        this.postStateId = postStateId;
    }

    public Transition getTransition() {
        return transition;
    }

    public void setTransition(Transition transition) {
        this.transition = transition;
    }
    
}
