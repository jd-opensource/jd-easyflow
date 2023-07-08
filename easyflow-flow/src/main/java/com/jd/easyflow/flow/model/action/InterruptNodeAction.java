package com.jd.easyflow.flow.model.action;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class InterruptNodeAction implements NodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        context.setInterrupted();
        return null;
    }

}
