package com.jd.easyflow.flow.model.action.compensate;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
public class FlowCompensateAction implements NodeAction {

    @Override
    public FlowResult execute(NodeContext nodeContext, FlowContext context) {
        FlowResult result = (FlowResult) nodeContext.getActionResult();
        FlowParam param = new FlowParam();
        param.setContext(result.getContext());
        CompensateHelper.compensate(result.getContext());
        FlowResult compensateResult = context.getFlowEngine().execute(param);
        return compensateResult;
    }

}
