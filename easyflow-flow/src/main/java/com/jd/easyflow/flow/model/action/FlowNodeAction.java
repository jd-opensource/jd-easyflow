package com.jd.easyflow.flow.model.action;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.parser.param.FlowParseParam;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * Sub flow NodeAction.
 * 
 * IMPORTANT NOTICE! This class should not be singleton!
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class FlowNodeAction implements NodeAction {

    private String flowId;
    private String[] startNodeIds;
    private Boolean inherit;

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
            param.setLogFlag(context.getParam().getLogFlag());
        }
        // init context.
        FlowContext subContext = new FlowContextImpl();
        
        if (inherit) {
            subContext.setData(context.getData());
            subContext.setLogFlag(context.getLogFlag());
        } else {
            subContext.put(FlowConstants.CTX_PARENT_CONTEXT, context);
        }
        subContext.put(FlowConstants.CTX_PARENT_NODE_CONTEXT, nodeContext);
        // init result.
        FlowResult result = new FlowResult();
        if (inherit) {
            result.setResult(context.getResult().getResult());
        }

        param.setContext(subContext);
        subContext.setResult(result);

        FlowResult subResult = engine.execute(param);
        if (inherit) {
            if (subResult != null && subResult.getContext() != null && subResult.getContext().isInterrupted()) {
                context.setInterrupted();
            }
        }
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

    @Override
    public void init(InitContext initContext, Object parent) {
        FlowNode node = (FlowNode) parent;
        if (flowId == null) {
            flowId = node.getProperty(DefConstants.COMMON_PROP_FLOW_ID);
            if (flowId == null) {
                Map<String, Object> flowDef = (Map<String, Object>) node.getProperty(DefConstants.COMMON_PROP_FLOW);
                if (flowDef != null) {
                    FlowParseParam param = new FlowParseParam();
                    param.setObjectDefinition(flowDef);
                    param.setParseEl(initContext.isParseEl());
                    List<Flow> flowList = initContext.getFlowParser().parse(param);
                    Flow subFlow = flowList.get(0);
                    initContext.getFlowList().add(subFlow);
                    flowId = subFlow.getId();
                }
            }
        }
        if (startNodeIds == null) {
            Object startNodeId = node.getProperty(DefConstants.NODE_ACTION_PROP_START_NODE_ID);
            if (startNodeId != null) {
                if (startNodeId instanceof String) {
                    startNodeIds = new String[] { (String) startNodeId };
                } else {
                    startNodeIds = ((List<String>) startNodeId).toArray(new String[] {});
                }
            }
        }

        if (inherit == null) {
            inherit = node.getProperty(DefConstants.NODE_ACTION_PROP_INHERIT);
        }
        if (inherit == null) {
            inherit = true;
        }
    }

}
