package com.jd.easyflow.flow.model.parser.event;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 */
public class ExpFlowParseEventListener implements FlowParseEventListener {
    
    private String exp;
    
    public ExpFlowParseEventListener() {
        
    }
    
    public ExpFlowParseEventListener(String exp) {
        this.exp = exp;
    }

    @Override
    public void on(FlowParseEvent event) {
        Map<String, Object> data =  new HashMap<>();
        data.put("event", event);
        event.getFlowParser().getElEvaluator().evalWithDefaultContext(exp, data, false);
    }

}
