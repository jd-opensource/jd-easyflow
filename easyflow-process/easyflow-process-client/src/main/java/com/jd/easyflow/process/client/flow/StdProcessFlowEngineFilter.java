package com.jd.easyflow.process.client.flow;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdProcessFlowEngineFilter extends StdProcessFlowListener implements Filter<Pair<FlowParam, FlowEngine>, FlowResult> {
    


    @Override
    public FlowResult doFilter(Pair<FlowParam, FlowEngine> param, FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain) {
        super.onFlowEngineStart(param.getLeft(), param.getRight());
        return chain.doFilter(param);
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
