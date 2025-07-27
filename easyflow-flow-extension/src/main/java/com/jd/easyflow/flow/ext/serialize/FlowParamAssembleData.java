package com.jd.easyflow.flow.ext.serialize;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
public class FlowParamAssembleData {

    private FlowContext flowContext;
    
    private NodeContext nodeContext;

    public FlowContext getFlowContext() {
        return flowContext;
    }

    public void setFlowContext(FlowContext flowContext) {
        this.flowContext = flowContext;
    }

    public NodeContext getNodeContext() {
        return nodeContext;
    }

    public void setNodeContext(NodeContext nodeContext) {
        this.nodeContext = nodeContext;
    }
    
    
}
