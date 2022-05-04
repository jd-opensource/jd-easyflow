package com.jd.easyflow.flow.cases.subflow;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class TestInvokeSubFlowNodeAction implements NodeAction {

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        String subFlowId = context.getFlow().getNode(nodeContext.getNodeId()).getProperty("subFlowId");
        FlowParam flowParam = new FlowParam(subFlowId);
        context.getFlowEngine().execute(flowParam);
        return null;
    }

}
