package com.jd.easyflow.flow.cases.performance;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class EmptyStepAction implements NodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        int i = 0;
        i++;
         return null;
    }

}
