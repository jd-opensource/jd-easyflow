package com.jd.easyflow.process.client.flow.util;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * @author liyuliang5
 */
public class StdProcessFlowUtil {
    
    public static StdProcessContext getStdProcessContext(FlowContext flowContext) {
        return flowContext.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
    }
    
    public static StdNodeContext getStdNodeContext(NodeContext nodeContext) {
        return nodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
    }
    
}
