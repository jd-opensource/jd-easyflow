package com.jd.easyflow.flow.model.action.compensate;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
public interface CompensateAction  {

    public <T>T compensate(NodeContext nodeContext, FlowContext flowContext);
    
}