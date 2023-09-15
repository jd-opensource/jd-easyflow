package com.jd.easyflow.flow.model.action;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * Event node action.
 * 
 * @author liyuliang5
 *
 */
public class EventNodeAction implements NodeAction {

    private static final Logger logger = LoggerFactory.getLogger(EventNodeAction.class);

    @Override
    public void init(InitContext initContext, FlowNode flowNode) {
        initEventActionMap(initContext, flowNode);
    }

    @Override
    public Object execute(NodeContext nodeContext, FlowContext context) {
        String event = nodeContext.get(FlowConstants.NODE_CONTEXT_DATA_EVENT);
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("Event is:" + event);
        }
        if (event == null) {
            event = FlowConstants.NONE_EVENT;
        }
        Map<String, NodeAction> eventActionMap = context.getFlow().getNode(nodeContext.getNodeId())
                .getProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP);
        // For exp scenario
        if (eventActionMap == null) {
            InitContext initContext = new InitContext();
            initContext.setFlowParser(context.getFlowEngine().getFlowParser());
            initContext.setParseEl(true);
            initEventActionMap(initContext, context.getFlow().getNode(nodeContext.getNodeId()));
            eventActionMap = context.getFlow().getNode(nodeContext.getNodeId())
                    .getProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP);
        }
        NodeAction nodeAction = eventActionMap.get(event);
        if (nodeAction == null) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Event node action is null");
            }
            return null;
        }
        return nodeAction.execute(nodeContext, context);
    }

    private void initEventActionMap(InitContext initContext, FlowNode flowNode) {
        Map<String, Object> eventActionMap = flowNode.getProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP);
        if (eventActionMap != null) {
            return;
        }
        eventActionMap = new ConcurrentHashMap<>();
        flowNode.setProperty(FlowConstants.PROP_RUNTIME_EVENT_NODE_ACTION_MAP, eventActionMap);
        Map<String, Object> map = flowNode.getProperty(FlowConstants.NODE_PROP_EVENTS);
        if (map == null) {
            return;
        }
        for (Entry<String, Object> entry : map.entrySet()) {
            String event = entry.getKey();
            Object eventConf = (Object) entry.getValue();
            Map<String, Object> eventActionConfMap = null;
            if (eventConf instanceof String) {
                eventActionConfMap = new HashMap<>();
                eventActionConfMap.put(DefConstants.COMMON_PROP_EXP, (String) eventConf);
            } else {
                Map<String, Object> eventConfMap = (Map<String, Object>) eventConf;
                eventActionConfMap = (Map<String, Object>) eventConfMap.get("action");
            }
            if (eventActionConfMap != null) {
                NodeAction nodeAction = initContext.getFlowParser()
                        .parseNodeAction(new ActionParseParam(eventActionConfMap, null, initContext.isParseEl(), flowNode));
                nodeAction.init(initContext, flowNode);
                if (nodeAction != null) {
                    eventActionMap.put(event, nodeAction);
                }
            }
        }

    }

}
