package com.jd.easyflow.fsm.model.check;

import com.jd.easyflow.fsm.Fsm;

/**
 * @author liyuliang5
 */
public class CheckErrorItem {

    String errorType;
    String errorMessage;
    String fsmId;
    String stateId;
    Fsm fsm;
    
    public CheckErrorItem() {
        
    }
    
    public CheckErrorItem(String errorType, String errorMessage, String fsmId, String stateId, Fsm fsm) {
        this.errorType = errorType;
        this.errorMessage = errorMessage;
        this.fsmId = fsmId;
        this.stateId = stateId;
        this.fsm = fsm;
    }

    public String getErrorType() {
        return errorType;
    }

    public void setErrorType(String errorType) {
        this.errorType = errorType;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public String getFsmId() {
        return fsmId;
    }

    public void setFsmId(String fsmId) {
        this.fsmId = fsmId;
    }

    public String getStateId() {
        return stateId;
    }

    public void setStateId(String stateId) {
        this.stateId = stateId;
    }

    public Fsm getFsm() {
        return fsm;
    }

    public void setFsm(Fsm fsm) {
        this.fsm = fsm;
    }
    
}
