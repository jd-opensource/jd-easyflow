package com.jd.easyflow.fsm.event;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.fsm.el.ElEvaluator;
import com.jd.easyflow.fsm.el.ElFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFsmEventListener implements FsmEventListener {
    
    private ElEvaluator elEvaluator;

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
        ElEvaluator elEvaluator = this.elEvaluator;
        if (elEvaluator == null && event.getContext() != null) {
            elEvaluator = event.getContext().getElEvaluator();
        }
        if (elEvaluator == null) {
            elEvaluator = ElFactory.get();
        }
        elEvaluator.eval(exp, null, event.getContext(), data);
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }
    
    
}
