package com.jd.easyflow.fsm.model.check;

import com.jd.easyflow.fsm.Fsm;

/**
 * @author liyuliang5
 */
public class CheckParam {
    
    private Fsm fsm;
    
    private Object config;

    public Fsm getFsm() {
        return fsm;
    }

    public void setFsm(Fsm fsm) {
        this.fsm = fsm;
    }

    public Object getConfig() {
        return config;
    }

    public void setConfig(Object config) {
        this.config = config;
    }
    
    

}
