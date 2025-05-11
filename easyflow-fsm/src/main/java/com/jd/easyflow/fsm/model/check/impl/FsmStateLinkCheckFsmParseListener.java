package com.jd.easyflow.fsm.model.check.impl;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.exception.FsmException;
import com.jd.easyflow.fsm.model.check.CheckErrorItem;
import com.jd.easyflow.fsm.model.check.CheckParam;
import com.jd.easyflow.fsm.model.check.CheckResult;
import com.jd.easyflow.fsm.parser.event.FsmParseEvent;
import com.jd.easyflow.fsm.parser.event.FsmParseEventListener;
import com.jd.easyflow.fsm.parser.event.FsmParseEventTypes;

/**
 * @author liyuliang5
 */
public class FsmStateLinkCheckFsmParseListener implements FsmParseEventListener {

    private static final Logger logger = LoggerFactory.getLogger(FsmStateLinkCheckFsmParseListener.class);

    public static final int CHECK_POLICY_OFF = 0;
    public static final int CHECK_POLICY_WARN = 1;
    public static final int CHECK_POLICY_EXCEPTION = 2;

    private boolean check = true;

    private int checkStateIsolatedPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNextStatesNotExistsPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonStartStateNoPreviousPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonEndStateNoNextPolicy = CHECK_POLICY_OFF;

    private FsmStateLinkChecker checker = new FsmStateLinkChecker();
    
    private Set<String> fsmWhitelist;
    
    private boolean checkOnlyParseEl = false;

    @Override
    public void on(FsmParseEvent event) {
        if (!check) {
            return;
        }
        switch (event.getType()) {
        case FsmParseEventTypes.INIT_FSM_END: {
            if (checkOnlyParseEl && ! event.isParseEl()) {
                return;
            }
            if (logger.isDebugEnabled()) {
                logger.debug("Start state link check, fsm ID:" + event.getFsm().getId());
            }
            if (fsmWhitelist != null && fsmWhitelist.contains(event.getFsm().getId())) {
                return;
            }
            CheckParam param = new CheckParam();
            param.setFsm(event.getFsm());
            FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
            config.setCheckStateIsolated(checkStateIsolatedPolicy != CHECK_POLICY_OFF);
            config.setCheckNextStatesNotExists(checkNextStatesNotExistsPolicy != CHECK_POLICY_OFF);
            config.setCheckNonStartStateNoPrevious(checkNonStartStateNoPreviousPolicy != CHECK_POLICY_OFF);
            param.setConfig(config);
            CheckResult result = checker.check(param);
            for (CheckErrorItem item : result.getErrorItemList()) {
                int policy;
                switch (item.getErrorType()) {
                case FsmStateLinkChecker.ERROR_TYPE_STATE_ISOLATED: {
                    policy = checkStateIsolatedPolicy;
                    break;
                }
                case FsmStateLinkChecker.ERROR_TYPE_NEXT_STATES_NOT_EXISTS: {
                    policy = checkNextStatesNotExistsPolicy;
                    break;
                }
                case FsmStateLinkChecker.ERROR_TYPE_NON_START_STATE_NO_PREVIOUS: {
                    policy = checkNonStartStateNoPreviousPolicy;
                    break;
                }
                case FsmStateLinkChecker.ERROR_TYPE_NON_END_STATE_NO_NEXT: {
                    policy = checkNonEndStateNoNextPolicy;
                    break;
                }
                default: {
                    policy = CHECK_POLICY_OFF;
                }
                }
                logger.warn("Fsm check error, type:" + item.getErrorType() + " flowId:" + item.getFsmId() + " stateId:"
                        + item.getStateId() + " fsm:\n" + item.getFsm().stringify());
                if (policy == CHECK_POLICY_EXCEPTION) {
                    throw new FsmException("Fsm check error, type:" + item.getErrorType() + " flowId:" + item.getFsmId()
                            + " stateId:" + item.getStateId());
                }
            }
        }
        }
    }

    public boolean isCheck() {
        return check;
    }

    public void setCheck(boolean check) {
        this.check = check;
    }

    public int getCheckStateIsolatedPolicy() {
        return checkStateIsolatedPolicy;
    }

    public void setCheckStateIsolatedPolicy(int checkStateIsolatedPolicy) {
        this.checkStateIsolatedPolicy = checkStateIsolatedPolicy;
    }

    public int getCheckNextStatesNotExistsPolicy() {
        return checkNextStatesNotExistsPolicy;
    }

    public void setCheckNextStatesNotExistsPolicy(int checkNextStatesNotExistsPolicy) {
        this.checkNextStatesNotExistsPolicy = checkNextStatesNotExistsPolicy;
    }

    public int getCheckNonStartStateNoPreviousPolicy() {
        return checkNonStartStateNoPreviousPolicy;
    }

    public void setCheckNonStartStateNoPreviousPolicy(int checkNonStartStateNoPreviousPolicy) {
        this.checkNonStartStateNoPreviousPolicy = checkNonStartStateNoPreviousPolicy;
    }

    public int getCheckNonEndStateNoNextPolicy() {
        return checkNonEndStateNoNextPolicy;
    }

    public void setCheckNonEndStateNoNextPolicy(int checkNonEndStateNoNextPolicy) {
        this.checkNonEndStateNoNextPolicy = checkNonEndStateNoNextPolicy;
    }

    public FsmStateLinkChecker getChecker() {
        return checker;
    }

    public void setChecker(FsmStateLinkChecker checker) {
        this.checker = checker;
    }

    public Set<String> getFsmWhitelist() {
        return fsmWhitelist;
    }

    public void setFsmWhitelist(Set<String> fsmWhitelist) {
        this.fsmWhitelist = fsmWhitelist;
    }

    public boolean isCheckOnlyParseEl() {
        return checkOnlyParseEl;
    }

    public void setCheckOnlyParseEl(boolean checkOnlyParseEl) {
        this.checkOnlyParseEl = checkOnlyParseEl;
    }
    
    
}
