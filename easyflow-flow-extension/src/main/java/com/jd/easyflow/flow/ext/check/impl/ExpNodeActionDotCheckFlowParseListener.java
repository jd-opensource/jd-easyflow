package com.jd.easyflow.flow.ext.check.impl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.action.EventNodeAction;
import com.jd.easyflow.flow.model.action.ExpNodeAction;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * Check the exp of ExpNodeAction contains dot to avoid '@xxxBean' format.
 * @author liyuliang5
 * 
 */
public class ExpNodeActionDotCheckFlowParseListener implements FlowParseEventListener {
    
    private static final Logger logger = LoggerFactory.getLogger(ExpNodeActionDotCheckFlowParseListener.class);

    private static final String NODE_ACTION_EXP_NO_DOT_POLICY_WARN = "WARN";
    private static final String NODE_ACTION_EXP_NO_DOT_POLICY_EXCEPTION = "EXCEPTION";

    private String nodeActionExpNoDotPolicy;

    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            if (logger.isDebugEnabled()) {
                logger.debug("Start flow check, flow ID:" + event.getFlow().getId());
            }
            Flow flow = event.getFlow();
            if (flow.getNodeList() != null) {
                for (FlowNode node : flow.getNodeList()) {
                    if (node instanceof NodeImpl) {
                        NodeImpl nodeImpl = (NodeImpl) node;
                        NodeAction nodeAction = nodeImpl.getAction();
                        if (nodeAction instanceof ExpNodeAction) {
                            // exp node action
                            validateNodeActionExp((ExpNodeAction) nodeAction, flow, nodeImpl);
                        } else if (nodeAction instanceof EventNodeAction) {
                            // event node action
                            Map<String, NodeAction> eventActionMap = node
                                    .getProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP);
                            if (eventActionMap != null) {
                                for (NodeAction subNodeAction : eventActionMap.values()) {
                                    if (subNodeAction instanceof ExpNodeAction) {
                                        validateNodeActionExp((ExpNodeAction) subNodeAction, flow, nodeImpl);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        }

    }

    private void validateNodeActionExp(ExpNodeAction expNodeAction, Flow flow, FlowNode node) {
        String exp = expNodeAction.getExp();
        if (!exp.contains(".")) {
            String errorMsg =  "[EXP INVALID]Flow:" + flow.getId() + " Node:" + node.getId() + " Exp:[" + exp + "] invalid，should contains dot";
            logger.warn(errorMsg);
            if (NODE_ACTION_EXP_NO_DOT_POLICY_EXCEPTION.equals(nodeActionExpNoDotPolicy)) {
                throw new FlowException(errorMsg);
            } 
        }
    }

    public String getNodeActionExpNoDotPolicy() {
        return nodeActionExpNoDotPolicy;
    }

    public void setNodeActionExpNoDotPolicy(String nodeActionExpNoDotPolicy) {
        this.nodeActionExpNoDotPolicy = nodeActionExpNoDotPolicy;
    }
    
    

}
