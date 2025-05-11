package com.jd.easyflow.fsm.model.check.impl;

import java.util.List;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.check.CheckErrorItem;
import com.jd.easyflow.fsm.model.check.CheckParam;
import com.jd.easyflow.fsm.model.check.CheckResult;
import com.jd.easyflow.fsm.model.check.FsmChecker;
import com.jd.easyflow.fsm.util.FsmStateLinkUtil;

/**
 * @author liyuliang5
 */
public class FsmStateLinkChecker implements FsmChecker {


    public static final String ERROR_TYPE_STATE_ISOLATED = "STATE_ISOLATED";
    public static final String ERROR_TYPE_NEXT_STATES_NOT_EXISTS = "NEXT_STATES_NOT_EXISTS";
    public static final String ERROR_TYPE_NON_START_STATE_NO_PREVIOUS = "NON_START_STATE_NO_PREVIOUS";
    public static final String ERROR_TYPE_NON_END_STATE_NO_NEXT = "NON_END_STATE_NO_NEXT";
    
    public CheckResult check(CheckParam param) {
        Fsm fsm = param.getFsm();
        FsmStateLinkCheckConfig config = (FsmStateLinkCheckConfig) param.getConfig();
        CheckResult result = new CheckResult();
        if (fsm.getStateList() == null) {
            return result;
        }
        for (State state : fsm.getStateList()) {
            List<String> previousStateIds = FsmStateLinkUtil.getPreviousStates(state, fsm);
            List<String> nextStateIds = FsmStateLinkUtil.getNextStates(state, fsm);
            // state isolated.
            if (config.isCheckStateIsolated()) {
                if (previousStateIds.size() == 0 && nextStateIds.size() == 0 && fsm.getStateList().size() > 1) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_STATE_ISOLATED, null, fsm.getId(), state.getId(), fsm));
                }
            }
            // next states none exists.
            if (config.isCheckNextStatesNotExists()) {
                if (nextStateIds.size() > 0) {
                    for (String stateId : nextStateIds) {
                        if (!stateId.equals(FsmStateLinkUtil.STATE_ID_UNKNOWN) && fsm.getState(stateId) == null) {
                            result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NEXT_STATES_NOT_EXISTS, null, fsm.getId(), state.getId(), fsm));
                        }
                    }
                }
            }
            // none start node has no previous.
            if (config.isCheckNonStartStateNoPrevious()) {
                if (previousStateIds.size() == 0 && ! state.getId().equals(fsm.getStartStateId())) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NON_START_STATE_NO_PREVIOUS, null, fsm.getId(), state.getId(), fsm));
                }
            }
            // none end node has no next.
            if (config.isCheckNonEndStateNoNext()) {
                if (nextStateIds.size() == 0 && ! Boolean.TRUE.equals(state.getProperty("end"))) {
                    result.addErrorItem(new CheckErrorItem(ERROR_TYPE_NON_END_STATE_NO_NEXT, null, fsm.getId(), state.getId(), fsm));
                }
            }            
            
        }
        return result;
    }

}
