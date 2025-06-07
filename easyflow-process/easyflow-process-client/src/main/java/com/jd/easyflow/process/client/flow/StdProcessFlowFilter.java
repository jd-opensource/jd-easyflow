package com.jd.easyflow.process.client.flow;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdProcessFlowFilter extends StdProcessFlowListener implements Filter<FlowContext, FlowResult> {

    @Override
    public FlowResult doFilter(FlowContext context, FilterChain<FlowContext, FlowResult> chain) {
        super.onFlowStart(context);
        try {
            FlowResult result = chain.doFilter(context);
            context.setResult(result);
            super.onFlowEnd(context);
            return context.getResult();
        } finally {
            super.onFlowComplete(context);
        }
    }
    
    protected int order;
    
    @Override
    public int getOrder() {
        return order;
    }
    
    public void setOrder(int order) {
        this.order = order;
    }

    @Override
    public void postConstruct(Map<String, Object> definition, Map<String, Object> context) {
        if (definition == null) {
            return;
        }
        Integer order = (Integer) definition.get("order");
        if (order != null) {
            this.order = order;
        }
    }

}
