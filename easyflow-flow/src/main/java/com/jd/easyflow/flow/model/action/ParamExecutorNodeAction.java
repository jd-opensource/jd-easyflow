package com.jd.easyflow.flow.model.action;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class ParamExecutorNodeAction implements NodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        NodeExecutor<Object> executor = context.getParam().get(FlowConstants.PARAM_ACTION_EXECUTOR);
        Object result = executor.execute(nodeContext, context);
        return (T) result;
    }

}
