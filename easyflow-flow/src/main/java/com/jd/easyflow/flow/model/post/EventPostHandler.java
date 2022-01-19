package com.jd.easyflow.flow.model.post;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class EventPostHandler implements NodePostHandler {
    
    private static final Logger logger = LoggerFactory.getLogger(EventPostHandler.class);
    
    @Override
    public void init(InitContext initContext, FlowNode flowNode) {
        initEventPostHandlerMap(initContext, flowNode);
    }

    @Override
    public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
        String event = nodeContext.get(FlowConstants.NODE_CONTEXT_DATA_EVENT);
        if (logger.isInfoEnabled()) {
            logger.info("Event:" + event);
        }
        if (event == null) {
            event = FlowConstants.NONE_EVENT;
        }

        Map<String, NodePostHandler> eventPostHandlerMap = context.getFlow().getNode(nodeContext.getNodeId())
                .getProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP);
        NodePostHandler postHandler = eventPostHandlerMap.get(event);
        if (postHandler == null) {
            if (logger.isInfoEnabled()) {
                logger.info("Event post handler is null");
            }
            return null;
        }
        return postHandler.postHandle(nodeContext, context);
    }
    
    private void initEventPostHandlerMap(InitContext initContext, FlowNode flowNode) {
        Map<String, Object> eventPostHandlerMap = flowNode.getProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP);
        if (eventPostHandlerMap != null) {
            return;
        }
        eventPostHandlerMap = new ConcurrentHashMap<String, Object>();
        flowNode.setProperty(FlowConstants.PROP_RUNTIME_EVENT_POST_HANDLER_MAP, eventPostHandlerMap);
        Map<String, Object> map = flowNode.getProperty(FlowConstants.NODE_PROP_EVENTS);
        if (map == null) {
            return;
        }
        for (Entry<String, Object> entry : map.entrySet()) {
            String event = entry.getKey();
            Object eventConf = (Object) entry.getValue();
            Map<String, Object> eventPostHandlerConfMap = null;
            if (eventConf instanceof Map) {
                Map<String, Object> eventConfMap = (Map<String, Object>) eventConf;
                eventPostHandlerConfMap = (Map<String, Object>) eventConfMap.get("post");
            }
            if (eventPostHandlerConfMap != null) {
                NodePostHandler nodePostHandler = initContext.getFlowParser().parsePost(eventPostHandlerConfMap, initContext.isParseEl());
                eventPostHandlerMap.put(event, nodePostHandler);
            }
        }
        
    }

}
