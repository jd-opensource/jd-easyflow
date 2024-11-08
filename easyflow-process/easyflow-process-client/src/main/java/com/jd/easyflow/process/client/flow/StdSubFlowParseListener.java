package com.jd.easyflow.process.client.flow;

import java.util.List;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.action.FlowNodeAction;
import com.jd.easyflow.flow.model.action.LoopNodeAction;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;

/**
 * 
 * @author liyuliang5
 */
public class StdSubFlowParseListener implements FlowParseEventListener {

    @Override
    public void on(FlowParseEvent event) {
        if (event.getType().equals(FlowParseEventTypes.INIT_FLOW_END)) {
            Flow flow = event.getFlow();
            List<FlowNode> flowNodeList = flow.getNodeList();
            for (FlowNode node : flowNodeList) {
                if (node instanceof NodeImpl) {
                    NodeAction nodeAction = ((NodeImpl) node).getAction();
                    if (nodeAction instanceof LoopNodeAction) {
                        nodeAction = ((LoopNodeAction) nodeAction).getLoopAction();
                    }
                    if (nodeAction instanceof FlowNodeAction) {
                        String flowId = ((FlowNodeAction) nodeAction).getFlowId();
                        if (!flowId.contains(StdFlowProcessConstants.VERSION_PREFIX)) {
                            flowId = flowId + StdFlowProcessConstants.VERSION_PREFIX;
                            ((FlowNodeAction) nodeAction).setFlowId(flowId);
                        }
                    }
                }
            }
        }
    }

}
