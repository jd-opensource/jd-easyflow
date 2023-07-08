package com.jd.easyflow.fsm.parser.event;

import java.util.Map;

import com.jd.easyflow.fsm.Fsm;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmParseEvent {
    
    private String type;
    
    private Map<String, Object> fsmDef;
    
    private Fsm fsm;
    
    private Object data;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Map<String, Object> getFsmDef() {
        return fsmDef;
    }

    public void setFsmDef(Map<String, Object> fsmDef) {
        this.fsmDef = fsmDef;
    }

    public Fsm getFsm() {
        return fsm;
    }

    public void setFsm(Fsm fsm) {
        this.fsm = fsm;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }
    
    
    
    
    

}
