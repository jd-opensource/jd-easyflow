package com.jd.easyflow.fsm.model.check.impl;

/**
 * @author liyuliang5
 */
public class FsmStateLinkCheckConfig {

    private boolean checkStateIsolated;
    
    private boolean checkNextStatesNotExists;
    
    private boolean checkNonStartStateNoPrevious;
    
    private boolean checkNonEndStateNoNext;


    public boolean isCheckStateIsolated() {
        return checkStateIsolated;
    }

    public void setCheckStateIsolated(boolean checkStateIsolated) {
        this.checkStateIsolated = checkStateIsolated;
    }

    public boolean isCheckNextStatesNotExists() {
        return checkNextStatesNotExists;
    }

    public void setCheckNextStatesNotExists(boolean checkNextStatesNotExists) {
        this.checkNextStatesNotExists = checkNextStatesNotExists;
    }

    public boolean isCheckNonStartStateNoPrevious() {
        return checkNonStartStateNoPrevious;
    }

    public void setCheckNonStartStateNoPrevious(boolean checkNonStartStateNoPrevious) {
        this.checkNonStartStateNoPrevious = checkNonStartStateNoPrevious;
    }

    public boolean isCheckNonEndStateNoNext() {
        return checkNonEndStateNoNext;
    }

    public void setCheckNonEndStateNoNext(boolean checkNonEndStateNoNext) {
        this.checkNonEndStateNoNext = checkNonEndStateNoNext;
    }
    
    
}
