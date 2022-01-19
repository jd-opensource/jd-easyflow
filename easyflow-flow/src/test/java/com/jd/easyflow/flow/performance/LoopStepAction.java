package com.jd.easyflow.flow.performance;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class LoopStepAction implements NodeAction {
    
    private static final String COUNT_KEY = "count";
    
    int i = 0;
    
    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        if (i < (int) context.getParam().get(COUNT_KEY)) {
            i++;
            nodeContext.setNextNodeIds(new String[] {"EMPTY_NODE3"});
        }
         return null;
    }

}

