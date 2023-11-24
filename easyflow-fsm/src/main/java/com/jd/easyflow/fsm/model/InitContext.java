package com.jd.easyflow.fsm.model;

import java.util.Map;

import com.jd.easyflow.fsm.Fsm;

/**
 * 
 * @author liyuliang5
 */
public class InitContext {


    private boolean parseEl;
        
    Fsm fsm;
    
    private Map<String, Object> fsmDefinitionMap;

    public boolean isParseEl() {
        return parseEl;
    }

    public void setParseEl(boolean parseEl) {
        this.parseEl = parseEl;
    }

    public Fsm getFsm() {
        return fsm;
    }

    public void setFsm(Fsm fsm) {
        this.fsm = fsm;
    }

    public Map<String, Object> getFsmDefinitionMap() {
        return fsmDefinitionMap;
    }

    public void setFsmDefinitionMap(Map<String, Object> fsmDefinitionMap) {
        this.fsmDefinitionMap = fsmDefinitionMap;
    }

}
