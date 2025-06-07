package com.jd.easyflow.flow.filter;

import java.util.Map;

/**
 * 
 */
public abstract class BaseFilter<T, R> implements Filter<T, R> {
    
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
