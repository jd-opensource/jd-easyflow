package com.jd.easyflow.flow.ext.check.impl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.action.EventNodeAction;
import com.jd.easyflow.flow.model.action.FlowNodeAction;
import com.jd.easyflow.flow.model.action.compensate.CompensateNodeFilter;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;
import com.jd.easyflow.flow.model.pre.InclusiveCheckPreHandler;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * @author liyuliang5
 */
public class SubFlowCheckFlowParseListener implements FlowParseEventListener {

    private static final Logger logger = LoggerFactory.getLogger(SubFlowCheckFlowParseListener.class);

    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            if (logger.isDebugEnabled()) {
                logger.debug("Start flow check, flow ID:" + event.getFlow().getId());
            }
            Flow flow = event.getFlow();
            boolean containsInheritFlowNodeAction = false;
            boolean containsInclusiveCheckPreHandler = false;
            boolean containsCompensateNodeFilter = false;
            if (flow.getNodeList() != null) {
                for (FlowNode node : flow.getNodeList()) {
                    if (node instanceof NodeImpl) {
                        NodeImpl nodeImpl = (NodeImpl) node;
                        if (nodeImpl.getPreHandler() instanceof InclusiveCheckPreHandler) {
                            containsInclusiveCheckPreHandler = true;
                        }
                        NodeAction nodeAction = nodeImpl.getAction();
                        if (nodeAction instanceof FlowNodeAction) {
                            FlowNodeAction flowNodeAction = (FlowNodeAction) nodeAction;
                            if (flowNodeAction.isInherit()) {
                                containsInheritFlowNodeAction = true;
                            }
                        } else if (nodeAction instanceof EventNodeAction) {
                            // event node action
                            Map<String, NodeAction> eventActionMap = node
                                    .getProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP);
                            if (eventActionMap != null) {
                                for (NodeAction subNodeAction : eventActionMap.values()) {
                                    if (subNodeAction instanceof FlowNodeAction) {
                                        FlowNodeAction flowNodeAction = (FlowNodeAction) subNodeAction;
                                        if (flowNodeAction.isInherit()) {
                                            containsInheritFlowNodeAction = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (flow.getFilterManager().getNodeFilters() != null) {
                for (Filter filter : flow.getFilterManager().getNodeFilters()) {
                    if (filter instanceof CompensateNodeFilter) {
                        containsCompensateNodeFilter = true;
                    }
                }
            }
            if (containsInheritFlowNodeAction) {
                if (containsInclusiveCheckPreHandler) {
                    throw new FlowException("FlowNodeAction with inherit conflict with InclusiveCheckPreHandler, set inherit to false");
                }
                if (containsCompensateNodeFilter) {
                    throw new FlowException("FlowNodeAction with inherit conflict with CompensateNodeFilter, set inherit to false");
                }
            }
        }
        }

    }
}
