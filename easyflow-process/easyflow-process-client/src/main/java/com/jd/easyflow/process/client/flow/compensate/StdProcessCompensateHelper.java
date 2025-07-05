package com.jd.easyflow.process.client.flow.compensate;

import java.util.ArrayList;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.compensate.CompensateHelper;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.flow.util.StdProcessFlowUtil;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * @author liyuliang5
 */
public class StdProcessCompensateHelper {

    static final String FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG = "_PROCESS_COMPENSATE_END_NODES_FLAG";

    
    static final String NODE_CONTEXT_COMPENSATE_NODE_FLAG = "_PROCESS_COMPENSATE_NODE_FLAG";
    static final String NODE_CONTEXT_COMPENSATE_FOR = "_PROCESS_COMPENSATE_FOR";

    public static NodeContext createCompensateNode(NodeContext nodeContext, ProcessRuntimeManager manager) {
         StdNodeContext stdNodeContext = StdProcessFlowUtil.getStdNodeContext(nodeContext);
         return createCompensateNode(stdNodeContext.getNodeInstanceNo(), stdNodeContext.getStdProcessContext(), manager);
    }
    
    
    public static NodeContext createCompensateNode(String nodeInstanceNo, StdProcessContext processContext, ProcessRuntimeManager manager) {
        ProcessNodeInstanceDTO nodeInstance = manager.getNodeInstance(nodeInstanceNo, processContext);
        NodeContext nodeCtx = new NodeContext(nodeInstance.getNodeId());
        nodeCtx.put(NODE_CONTEXT_COMPENSATE_NODE_FLAG, true);
        nodeCtx.put(NODE_CONTEXT_COMPENSATE_FOR, nodeInstanceNo);
        nodeCtx.put(StdFlowProcessConstants.FLOW_NODE_CTX_PROCESS_PRE_CHECK_TYPE, StdFlowProcessConstants.FLOW_NODE_CTX_PROCESS_PRE_CHECK_TYPE_NONE);
        nodeCtx.put(StdFlowProcessConstants.FLOW_NODE_CTX_PROCESS_PRE_CHECK_NODES, new ArrayList(0));
        
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
