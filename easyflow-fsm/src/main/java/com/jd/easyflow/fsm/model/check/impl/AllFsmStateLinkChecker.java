package com.jd.easyflow.fsm.model.check.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.SmartLifecycle;

import com.jd.easyflow.fsm.CoreFsmManager;
import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.exception.FsmException;
import com.jd.easyflow.fsm.model.check.CheckErrorItem;
import com.jd.easyflow.fsm.model.check.CheckParam;
import com.jd.easyflow.fsm.model.check.CheckResult;

/**
 * @author liyuliang5
 */
public class AllFsmStateLinkChecker  implements SmartLifecycle {
    
    private static final Logger logger = LoggerFactory.getLogger(AllFsmStateLinkChecker.class);
    

    public static final int CHECK_POLICY_OFF = 0;
    public static final int CHECK_POLICY_WARN = 1;
    public static final int CHECK_POLICY_EXCEPTION = 2;

    private boolean check = true;

    private int checkStateIsolatedPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNextStatesNotExistsPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonStartStateNoPreviousPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonEndStateNoNextPolicy = CHECK_POLICY_OFF;
    
    private static final String VERSION_PREFIX = "--V_";

    private FsmStateLinkChecker checker = new FsmStateLinkChecker();
    
    private Set<String> fsmWhitelist;
    

    private  int phase = Integer.MIN_VALUE + 1;
    
    private boolean autoStartup = true;

    private volatile boolean isRunning = false;
    
    private CoreFsmManager fsmManager;
    
    public void check() {
        List<CheckErrorItem> itemList = doCheck();
        boolean exception = false;
        for (CheckErrorItem item : itemList) {
            logger.warn("Fsm check error, type:" + item.getErrorType() + " flowId:" + item.getFsmId() + " stateId:"
                    + item.getStateId() + " fsm:\n" + item.getFsm().stringify());
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
            if (!exception && policy == CHECK_POLICY_EXCEPTION) {
                exception = true;
            }
        }
        if (exception) {
            throw new FsmException("Fsm check error!");
        }
    }
    
    public List<CheckErrorItem> doCheck() {
        List<Fsm> allFsmList = getAllFsmList();
        return doCheck(allFsmList);
    }
    
    public List<CheckErrorItem> doCheck(List<Fsm> allFsmList) {
        List<CheckErrorItem> list = new ArrayList<CheckErrorItem>();
        if (! check) {
            logger.info("check flag is false, return.");
            return list;
        }
        logger.info("start do fsm check");
        CheckParam param = new CheckParam();
        FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
        config.setCheckStateIsolated(checkStateIsolatedPolicy != CHECK_POLICY_OFF);
        config.setCheckNextStatesNotExists(checkNextStatesNotExistsPolicy != CHECK_POLICY_OFF);
        config.setCheckNonStartStateNoPrevious(checkNonStartStateNoPreviousPolicy != CHECK_POLICY_OFF);
        param.setConfig(config);
        for (Fsm fsm : allFsmList) {
            if (fsmWhitelist != null && fsmWhitelist.contains(fsm.getId())) {
                continue;
            }
            param.setFsm(fsm);
            CheckResult checkResult = checker.check(param);
            if (checkResult.getErrorItemList() != null && checkResult.getErrorItemList().size() > 0) {
                list.addAll(checkResult.getErrorItemList());
            }
        }
        logger.info("end do fsm check, fsm size:" + allFsmList.size() + ", error size:" + list.size());
        return list;
    }
    
    protected List<Fsm> getAllFsmList() {
        List<Fsm> list = new ArrayList<Fsm>();
        for (Entry<String, Fsm> entry : fsmManager.getFsmMap().entrySet()) {
            if (entry.getKey().endsWith(VERSION_PREFIX)) {
                continue;
            }
            list.add(entry.getValue());
        }
        return list;
    }
    

    @Override
    public void start() {
        check();
        isRunning = true;
    }

    @Override
    public void stop() {
        isRunning = false;
    }
    
    @Override
    public boolean isAutoStartup() {
        return autoStartup;
    }
    
    @Override
    public void stop(Runnable callback) {
        stop();
        callback.run();
    }

    @Override
    public boolean isRunning() {
        return isRunning;
    }
    
    @Override
    public int getPhase() {
        return phase;
    }

    public void setPhase(int phase) {
        this.phase = phase;
    }

    public void setAutoStartup(boolean autoStartup) {
        this.autoStartup = autoStartup;
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

    public CoreFsmManager getFsmManager() {
        return fsmManager;
    }

    public void setFsmManager(CoreFsmManager fsmManager) {
        this.fsmManager = fsmManager;
    }


    
}
