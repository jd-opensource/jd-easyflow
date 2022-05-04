package com.jd.easyflow.flow.model.action;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * Sub flow NodeAction
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class FlowNodeAction implements NodeAction {

    public static final String PARENT_NODE_CONTEXT = "parentNodeContext";
    public static final String PARENT_CONTEXT = "parentContext";

    private String flowId;
    private String[] startNodeIds;
    private boolean inherit = true;
    
    public FlowNodeAction() {
        // NOOP
    }
    
    public FlowNodeAction(String flowId, String[] startNodeIds, boolean inherit) {
        this.flowId = flowId;
        this.startNodeIds = startNodeIds;
        this.inherit = inherit;
    }

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        FlowEngine engine = context.getFlowEngine();
        // init param.
        FlowParam param = new FlowParam();
        param.setFlowId(flowId);
        param.setNodeIds(startNodeIds);
        if (inherit) {
            param.setParam(context.getParam().getParam());
        }
        // init context.
        FlowContext subContext = new FlowContextImpl();
        if (inherit) {
            subContext.setData(context.getData());
        } else {
            subContext.put(PARENT_CONTEXT, context);
        }
        subContext.put(PARENT_NODE_CONTEXT, nodeContext);
        // init result.
        FlowResult result = new FlowResult();
        if (inherit) {
            result.setResult(context.getResult().getResult());
        }

        param.setContext(subContext);
        subContext.setResult(result);

        FlowResult subResult = engine.execute(param);
        return (T) subResult;
    }

    public String getFlowId() {
        return flowId;
    }

    public void setFlowId(String flowId) {
        this.flowId = flowId;
    }


    public String[] getStartNodeIds() {
        return startNodeIds;
    }

    public void setStartNodeIds(String[] startNodeIds) {
        this.startNodeIds = startNodeIds;
    }

    public boolean isInherit() {
        return inherit;
    }

    public void setInherit(boolean inherit) {
        this.inherit = inherit;
    }

}
