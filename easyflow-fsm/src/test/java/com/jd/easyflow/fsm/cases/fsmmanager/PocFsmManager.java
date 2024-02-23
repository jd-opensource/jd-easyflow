package com.jd.easyflow.fsm.cases.fsmmanager;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;

/**
 * 
 * @author liyuliang5
 */
public abstract class PocFsmManager {

    protected Map<String, Fsm> fsmMap = new ConcurrentHashMap<>();

    private String fsmPath;


    public FsmResult run(FsmParam param) {
            return invokeFsm(param);
    }

    protected FsmResult invokeFsm(FsmParam param) {
            Fsm fsm = fsmMap.get(param.getFsmId());
            FsmResult result = fsm.run(param);
            return result;
    }

    public String getFsmPath() {
        return fsmPath;
    }

    public void setFsmPath(String fsmPath) {
        this.fsmPath = fsmPath;
    }

    public Map<String, Fsm> getFsmMap() {
        return fsmMap;
    }

    public void setFsmMap(Map<String, Fsm> fsmMap) {
        this.fsmMap = fsmMap;
    }

}
