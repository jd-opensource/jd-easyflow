package com.jd.easyflow.process.client.flow.util;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
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
    
    public static ProcessInstanceDTO getCachedProcessInstance(FlowContext flowContext) {
        StdProcessContext processContext = getStdProcessContext(flowContext);
        if (processContext == null) {
            return null;
        }
        ProcessInstanceDTO instance = processContext.getCache().get(ProcessInstanceDTO.class, processContext.getInstanceNo());
        return instance;
    }
    
    public static ProcessNodeInstanceDTO getCachedNodeInstance(NodeContext nodeContext, FlowContext flowContext) {
        StdProcessContext processContext = getStdProcessContext(flowContext);
        if (processContext == null) {
            return null;
        }
        StdNodeContext stdNodeContext = getStdNodeContext(nodeContext);
        if (stdNodeContext == null) {
            return null;
        }
        ProcessNodeInstanceDTO nodeInstance = processContext.getCache().get(ProcessNodeInstanceDTO.class, stdNodeContext.getNodeInstanceNo());
        return nodeInstance;
    }
    
}
