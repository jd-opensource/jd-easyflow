package com.jd.easyflow.flow.engine.event;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.flow.el.ElFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFlowEventListener implements FlowEventListener {
    
    private String exp;
    
    public ExpFlowEventListener() {
        
    }
    
    public ExpFlowEventListener(String exp) {
        this.exp = exp;
    }

    @Override
    public void on(FlowEvent flowEvent) {
        Map<String, Object> data =  new HashMap<>();
        data.put("event", flowEvent);
        ElFactory.get().eval(exp, null, flowEvent.getContext(), data);
    }

}
