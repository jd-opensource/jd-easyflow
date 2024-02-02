package com.jd.easyflow.flow.engine.event;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.flow.el.ElEvaluator;
import com.jd.easyflow.flow.el.ElFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFlowEventListener implements FlowEventListener {
    
    private ElEvaluator elEvaluator;
    
    private String exp;
    
    public ExpFlowEventListener() {
        
    }
    
    public ExpFlowEventListener(String exp) {
        this.exp = exp;
    }
    
    @Override
    public void on(FlowEvent flowEvent) {
        Map<String, Object> data = new HashMap<>();
        data.put("event", flowEvent);
        ElEvaluator evaluator = elEvaluator;
        if (evaluator == null && flowEvent.getContext() != null) {
            elEvaluator = flowEvent.getContext().getElEvaluator();
        }
        if (evaluator == null) {
            evaluator = ElFactory.get();
        }
        evaluator.eval(exp, null, flowEvent.getContext(), data);
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }

    public String getExp() {
        return exp;
    }

    public void setExp(String exp) {
        this.exp = exp;
    }
    
    

}
