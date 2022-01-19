package com.jd.easyflow.fsm.event;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.fsm.el.ElFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFsmEventListener implements FsmEventListener {

    private String exp;
    
    public ExpFsmEventListener() {
        
    }
    
    public ExpFsmEventListener(String exp) {
        this.exp = exp;
    }

    @Override
    public void on(FsmEvent event) {
        Map<String, Object> data = new HashMap<>();
        data.put("event", event);
        ElFactory.get().eval(exp, null, event.getContext(), data);
    }
    
    
    
}
