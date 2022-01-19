package com.jd.easyflow.flow.engine.impl;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowRunner;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFlowRunner implements FlowRunner{
    
    private String exp;
    
    public ExpFlowRunner() {
        
    }
    
    public ExpFlowRunner(String exp) {
        this.exp = exp;
    }

    @Override
    public void run(FlowContext context) {
        ElFactory.get().eval(exp, null, context, null);
    }

}
