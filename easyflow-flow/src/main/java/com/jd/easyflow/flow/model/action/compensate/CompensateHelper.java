package com.jd.easyflow.flow.model.action.compensate;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * Unstable!!! Only for memory model!!!
 * @author liyuliang5
 */
public class CompensateHelper {
    
    private static final String FLOW_CONTEXT_COMPENSATE_FLAG = "_compensateFlag";
    static final String FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG = "_compensateEndNodesFlag";

    
    static final String NODE_CONTEXT_COMPENSATING_FLAG = "_compensatingFlag";

    public static void compensate(FlowContext context) {
        context.put(FLOW_CONTEXT_COMPENSATE_FLAG, true);
    }
    
    public static boolean isCompensating(FlowContext context) {
        return Boolean.TRUE.equals(context.get(FLOW_CONTEXT_COMPENSATE_FLAG));
    }
    
    
    public static NodeContext createCompensateNode(NodeContext nodeContext) {
        NodeContext nodeCtx = new NodeContext(nodeContext.getNodeId());
        nodeCtx.put(FlowConstants.NODECTX_COMPENSATE_NODE_FLAG, true);
        nodeCtx.put(FlowConstants.NODECTX_COMPENSATE_FOR, nodeContext);
        nodeContext.put(FlowConstants.NODECTX_COMPENSATED_BY, nodeCtx);
        return nodeCtx;
    }
    
    public static FlowResult compensateFlow(FlowContext context) {
        FlowParam compensateParam = new FlowParam();
        CompensateHelper.compensate(context);
        compensateParam.setContext(context);
        FlowResult compensateResult = context.getFlowEngine().execute(compensateParam);
        return compensateResult;
    }
    
    
}