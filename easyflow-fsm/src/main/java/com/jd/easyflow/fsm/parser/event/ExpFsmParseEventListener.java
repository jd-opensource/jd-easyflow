package com.jd.easyflow.fsm.parser.event;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public class ExpFsmParseEventListener implements FsmParseEventListener {
    
    private String exp;
    
    public ExpFsmParseEventListener() {
        
    }
    
    public ExpFsmParseEventListener(String exp) {
        this.exp = exp;
    }

    @Override
    public void on(FsmParseEvent event) {
        Map<String, Object> data =  new HashMap<>();
        data.put("event", event);
        event.getElEvaluator().evalWithDefaultContext(exp, data, false);   
    }

}
